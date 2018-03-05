import Array exposing (Array)
import Set exposing (Set)
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Time
import Task
import WebSocket

import JsonFudge exposing (serialiseBundle, parseBundle)
import ClTypes exposing (..)
import ClNodes exposing (..)
import ClMsgTypes exposing (FromRelayClientBundle, ToRelayClientBundle(..), SubMsg(MsgSub), ErrorIndex(..))
import Futility exposing (..)
import PathManipulation exposing (appendSeg)
import RelayState exposing (..)
import MonoTime
import Layout exposing (Layout(..), LayoutPath, updateLayout, viewEditLayout, viewLayout, layoutRequires, LayoutEditEvent)
import Form exposing (FormStore, formStoreEmpty, FormUiEvent, formClear, formUiUpdate, FormEvent(..), FormState(..), formState, AtomEditState(..), castAes, UnboundFui(..), mapUfui, bindFui, castFormState)

main = Html.program {
    init = init, update = update, subscriptions = subscriptions, view = view}

wsTarget : String
wsTarget = "ws://localhost:8004"

-- Model

type UiMode
  = UmEdit
  | UmView

type alias NeConstT = List (Maybe WireValue)

type NodeEditEvent v
  = NeeSubmit v
  | NeeChildChoose

mapNee : (a -> b) -> NodeEditEvent a -> NodeEditEvent b
mapNee f e = case e of
    NeeSubmit v -> NeeSubmit <| f v
    NeeChildChoose -> NeeChildChoose

mapNeeUf : (a -> b) -> UnboundFui a (NodeEditEvent a) -> UnboundFui b (NodeEditEvent b)
mapNeeUf f = mapUfui f (mapNee f)

type NeChildMod
  = NcmPresentAfter Seg
  | NcmAbsent

type alias NeChildState =
  { chosen : Bool
  , mod : Maybe NeChildMod
  }

type alias NeChildrenT = Dict Seg NeChildState

type NodeEdit
  = NeConst NeConstT
  -- FIXME: Rename to NeChildren
  | NeChildren NeChildrenT

asNeConst : NodeEdit -> Result String NeConstT
asNeConst edit = case edit of
    NeConst e -> Ok e
    _ -> Err "Not NeConst"

asNeChildren : NodeEdit -> Result String NeChildrenT
asNeChildren edit = case edit of
    NeChildren e -> Ok e
    _ -> Err "Not NeChildren"

type alias NodeFs = FormStore Path NodeEdit (NodeEditEvent NodeEdit)

type alias Model =
  -- Global:
  { errs : List (ErrorIndex, String)
  , viewMode : UiMode
  -- Layout:
  , layout : Layout Path
  , layoutFs : FormStore LayoutPath Path (LayoutEditEvent Path)
  -- Data:
  , subs : Set Path
  , types : TypeMap
  , tyAssns : TypeAssignMap
  , nodes : NodeMap
  , nodeFs : NodeFs
  }

-- FIXME: Doesn't take edits into account
picksSegs : Maybe (List Seg) -> FormState NeChildrenT r -> (NeChildrenT, List Seg)
picksSegs mRemoteSegs s =
  let
    remoteSegs = Maybe.withDefault [] mRemoteSegs
    defaultPicks = case remoteSegs of
        (s :: _) -> Dict.singleton s <| NeChildState True Nothing
        [] -> Dict.empty
    formStateVal = case s of
        FsViewing -> defaultPicks
        FsEditing v -> v
        FsPending _ mv -> case mv of
            Nothing -> defaultPicks
            Just v -> v
  in (formStateVal, remoteSegs)


chosenChildPaths : NodeMap -> NodeFs -> Path -> Array Path
chosenChildPaths nm fs p =
  let
    mChildSegs = case Dict.get p nm of
        Nothing -> Nothing
        Just n -> case n of
            ContainerNode segs -> Just segs
            _ -> Nothing
  in case castFormState asNeChildren <| formState p fs of
        Ok neFs ->
          let
            (picks, segs) = picksSegs mChildSegs neFs
            isChosen seg = Maybe.withDefault False <| Maybe.map (.chosen) <| Dict.get seg picks
          in Array.fromList <| List.map (appendSeg p) <| List.filter isChosen segs
        Err _ -> Array.empty

requiredPaths : NodeMap -> NodeFs -> Layout Path -> Set Path
requiredPaths nm fs = layoutRequires (++) (dynamicLayout nm) (chosenChildPaths nm fs)

init : (Model, Cmd Msg)
init =
  let
    initialNodes = Dict.empty
    initialNodeFs = formStoreEmpty
    initialLayout = LayoutContainer <| Array.fromList [LayoutLeaf "/relay/self", LayoutChildChoice "/relay/clients" <| LayoutLeaf ""]
    initialSubs = requiredPaths initialNodes initialNodeFs initialLayout
    initialModel =
      { errs = []
      , viewMode = UmEdit
      , layout = initialLayout
      , layoutFs = formStoreEmpty
      , subs = initialSubs
      , types = Dict.empty
      , tyAssns = Dict.empty
      , nodes = initialNodes
      , nodeFs = initialNodeFs
      }
  in (initialModel, subDiffToCmd Set.empty initialSubs)

-- Update

sendBundle : ToRelayClientBundle -> Cmd Msg
sendBundle b = timeStamped (WebSocket.send wsTarget << serialiseBundle b)

subDiffToCmd : Set Path -> Set Path -> Cmd Msg
subDiffToCmd old new =
  let
    subToCmd ps = case ps of
        [] -> Cmd.none
        _ -> sendBundle (ToRelayClientBundle (List.map MsgSub ps) [] [])
  in if old == new
    then Cmd.none
    -- FIXME: Not efficient and leaks.
    else subToCmd <| Set.toList new

type Msg
  = AddError ErrorIndex String
  | SwapViewMode
  | NetworkEvent FromRelayClientBundle
  | TimeStamped (Time -> Cmd Msg) Time.Time
  | LayoutUiEvent (FormUiEvent LayoutPath Path (LayoutEditEvent Path))
  | NodeUiEvent (FormUiEvent Path NodeEdit (NodeEditEvent NodeEdit))

timeStamped : (Time -> Cmd Msg) -> Cmd Msg
timeStamped c = Task.perform (TimeStamped c) MonoTime.now

addGlobalError : String -> Model -> (Model, Cmd Msg)
addGlobalError msg m = ({m | errs = (GlobalError, msg) :: .errs m}, Cmd.none)

feHandler : Model -> FormEvent k r -> (k -> r -> (Model, Cmd Msg)) -> (Model, Cmd Msg)
feHandler m fe submitHandler = case fe of
    FeNoop -> (m, Cmd.none)
    FeError msg -> addGlobalError msg m
    FeAction k r -> submitHandler k r

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    AddError idx msg -> ({model | errs = (idx, msg) :: .errs model}, Cmd.none)
    NetworkEvent b ->
      let
        (newNodes, newAssns, newTypes, errs) = handleFromRelayBundle b (.nodes model) (.tyAssns model) (.types model)
        subs = requiredPaths newNodes (.nodeFs model) (.layout model)
      in
        ({model | nodes = newNodes, tyAssns = newAssns, types = newTypes, errs = errs ++ .errs model, subs = subs}, subDiffToCmd (.subs model) subs)
    TimeStamped c t -> (model, c (fromFloat t))
    SwapViewMode -> case .viewMode model of
        UmEdit -> ({model | viewMode = UmView}, Cmd.none)
        UmView -> ({model | viewMode = UmEdit}, Cmd.none)
    LayoutUiEvent fue ->
      let
        (newLayoutFs, fe) = formUiUpdate fue <| .layoutFs model
        newM = {model | layoutFs = newLayoutFs}
      in feHandler newM fe <| \lp r -> case updateLayout lp r <| .layout model of
        Err msg -> addGlobalError msg newM
        Ok newLayout ->
          let
            editProcessedFs = formClear lp <| .layoutFs newM
            subs = requiredPaths (.nodes newM) (.nodeFs newM) newLayout
          in ({newM | layout = newLayout, layoutFs = editProcessedFs, subs = subs}, subDiffToCmd (.subs newM) subs)
    NodeUiEvent fue ->
      let
        (newNodeFs, fe) = formUiUpdate fue <| .nodeFs model
        newM = {model | nodeFs = newNodeFs}
      in feHandler newM fe <| \p r -> case r of
        NeeChildChoose ->
          let
            subs = requiredPaths (.nodes newM) (.nodeFs newM) (.layout newM)
          in ({newM | subs = subs}, subDiffToCmd (.subs newM) subs)
        NeeSubmit v -> addGlobalError "Node edit not implemented" newM

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model = WebSocket.listen wsTarget eventFromNetwork

eventFromNetwork : String -> Msg
eventFromNetwork s = case parseBundle s of
    (Ok b) -> NetworkEvent b
    (Err e) -> AddError GlobalError e

-- View

dynamicLayout : NodeMap -> Path -> Layout Path
dynamicLayout nm p = case Dict.get p nm of
    Nothing -> LayoutLeaf p
    Just n -> case n of
        ContainerNode segs -> LayoutContainer <| Array.map (\seg -> dynamicLayout nm (p ++ "/" ++ seg)) <| Array.fromList segs
        _ -> LayoutLeaf p

view : Model -> Html Msg
view m = div []
  [ viewErrors <| .errs m
  , button [onClick SwapViewMode] [text "switcheroo"]
  , case .viewMode m of
    UmEdit -> Html.map LayoutUiEvent <| viewEditLayout "" pathEditView (.layoutFs m) (.layout m)
    UmView -> Html.map NodeUiEvent <| viewLayout
        (++)
        (dynamicLayout <| .nodes m)
        (chosenChildPaths (.nodes m) (.nodeFs m))
        (viewPath (.nodeFs m) (.types m) (.tyAssns m) (.nodes m))
        (.layout m)
  ]

viewErrors : List (ErrorIndex, String) -> Html a
viewErrors errs = ul [] (List.map (\s -> li [] [text <| toString s]) errs)

pathEditView : (Path -> r) -> Maybe Path -> FormState Path r -> Html (UnboundFui Path r)
pathEditView r mp fs = case fs of
    FsViewing -> case mp of
        Nothing -> text "Attempting to view unfilled path"
        Just p -> span [onClick <| UfUpdate p] [text p]
    FsEditing partial ->
      let
        buttonText = case mp of
            Nothing -> "Set"
            Just p -> "Replace " ++ p
      in Html.span []
        [ button [onClick <| UfAct <| r partial] [text buttonText]
        , input [value partial, type_ "text", onInput <| UfUpdate] []
        ]
    FsPending _ mPending ->
      let
        oldValStr = case mp of
            Nothing -> ""
            Just p -> p
      in case mPending of
        Nothing -> text "No pending, bad times"
        Just pending -> span
            [onClick <| UfUpdate pending]
            [text <| oldValStr ++ " -> " ++ pending]

viewPath : NodeFs -> TypeMap -> TypeAssignMap -> NodeMap -> Path -> Html (FormUiEvent Path NodeEdit (NodeEditEvent NodeEdit))
viewPath fs types tyAssns nodes p = case Dict.get p tyAssns of
    Nothing -> text <| "Loading type info: " ++ p
    Just (tn, lib) -> case Dict.get tn types of
        Nothing -> text <| "Type missing from map: " ++ toString tn
        Just ty -> Html.map (bindFui p) <| viewNode lib ty (Dict.get p nodes) <| formState p fs

viewCasted : (a -> Result String b) -> (b -> Html r) -> a -> Html r
viewCasted c h a = case c a of
    Ok b -> h b
    Err m -> text m

viewNode
   : Liberty -> Definition -> Maybe Node
   -> FormState NodeEdit (NodeEditEvent NodeEdit)
   -> Html (UnboundFui NodeEdit (NodeEditEvent NodeEdit))
viewNode lib def maybeNode formState =
  let
    rcns nc cn cfs h = viewCasted
        (\(n, s) -> Result.map2 (,) (castMaybe cn n) (castFormState cfs s))
        (Html.map (mapNeeUf nc) << uncurry h)
        (maybeNode, formState)
  in case (lib, def) of
    (Cannot, TupleDef d) -> viewCasted (castMaybe asConstDataNode) (viewConstTuple d) maybeNode
    (_, TupleDef d) -> rcns NeConst asConstDataNode asNeConst <| viewConstNodeEdit d
    (_, StructDef d) -> text "Struct edit not implemented"
    (Cannot, ArrayDef d) -> rcns NeChildren asContainerNode asNeChildren <| viewArray d
    (_, ArrayDef d) -> text "Array edit not implemented"

viewArray
   : ArrayDefinition
  -> Maybe ContainerNodeT -> FormState NeChildrenT (NodeEditEvent a)
  -> Html (UnboundFui NeChildrenT (NodeEditEvent NeChildrenT))
viewArray arrayDef mn s =
  let
    (picks, segs) = picksSegs mn s
    fillChoice seg isPicked mc = case mc of
        Nothing -> Just <| {chosen = isPicked, mod = Nothing}
        Just a -> Just <| {a | chosen = isPicked}
    wrapChoice (seg, isPicked) = UfActUp NeeChildChoose <| Dict.update seg (fillChoice seg isPicked) picks
  in Html.map wrapChoice <| viewChildrenChoose segs <| Dict.map (always .chosen) picks

viewChildrenChoose : List Seg -> Dict Seg Bool -> Html (Seg, Bool)
viewChildrenChoose segs chosen =
  let
    selWidget seg = case Maybe.withDefault False <| Dict.get seg chosen of
        True -> Html.li [onClick (seg, False)] [Html.b [] [Html.text seg]]
        False -> Html.li [onClick (seg, True)] [Html.text seg]
  in Html.ol [] <| List.map selWidget segs

viewConstTuple : TupleDefinition -> Maybe ConstDataNodeT -> Html a
viewConstTuple td mn = case mn of
    Nothing -> text "Awaiting data"
    Just n -> span [] <| List.map2
        (viewAtom <| Tuple.first <| .values n)
        (List.map Tuple.second <| .types td)
        (Tuple.second <| .values n)

viewAtom : Maybe Attributee -> AtomDef -> WireValue -> Html a
viewAtom ma def wv =
  let
    castedView : (WireValue -> Result String b) -> (Maybe Attributee -> b -> Html a) -> Html a
    castedView c h = case c wv of
        Ok a -> h ma a
        Err msg -> text msg
  in case def of
    ADEnum opts -> castedView asWord8 <| enumViewer opts
    ADFloat bounds -> castedView asFloat <| floatViewer bounds
    ADString re -> castedView asString <| textViewer re
    ADRef ty -> castedView asString <| refViewer ty
    _ -> text <| "View not implemented: " ++ toString def

viewConstNodeEdit
   : TupleDefinition-> Maybe ConstDataNodeT -> FormState NeConstT (NodeEditEvent a)
  -> Html (UnboundFui NeConstT (NodeEditEvent NeConstT))
viewConstNodeEdit d mn s = viewConstTupleEdit (List.map Tuple.second <| .types d) (Maybe.map (Tuple.second << .values) mn) s

viewConstTupleEdit
   : List AtomDef -> Maybe (List WireValue) -> FormState NeConstT (NodeEditEvent a)
   -> Html (UnboundFui NeConstT (NodeEditEvent NeConstT))
viewConstTupleEdit defs mv s =
  let
    nDefs = List.length defs
    current = case mv of
        Nothing -> List.repeat nDefs Nothing
        Just v -> List.map Just v
    toAes mev = case mev of
        Nothing -> AesUnfilled
        Just ev -> AesEditing ev
    (editBase, atomEditStates) = case s of
        FsViewing -> (current, List.repeat nDefs AesViewing)
        FsEditing mevs -> (mevs, List.map toAes mevs)
        FsPending _ mmevs -> case mmevs of
            Nothing -> (current, List.repeat nDefs AesViewing)
            Just mevs -> (mevs, List.map toAes mevs)
    asPartial idx wv = UfUpdate <| Result.withDefault editBase <| replaceIdx idx (Just wv) editBase
    atomEditor idx def mwv aes = Html.map (asPartial idx) <| viewAtomEdit def mwv aes
    atomEditors = List.map4 atomEditor (List.range 0 nDefs) defs current atomEditStates
    filledFields = List.filterMap identity editBase
    content = if List.length filledFields == List.length defs
      then button [onClick <| UfAct <| NeeSubmit editBase] [text "Apply"] :: atomEditors
      else atomEditors
  in span [] content

viewAtomEdit : AtomDef -> Maybe WireValue -> AtomEditState WireValue -> Html WireValue
viewAtomEdit d =
  let
    castedView
       : (WireValue -> Result String a) -> (a -> WireValue) -> (Maybe a -> AtomEditState a -> Html a)
       -> Maybe WireValue -> AtomEditState WireValue -> Html WireValue
    castedView toA toWv h mwv swv = Html.map toWv <| case castMaybe toA mwv of
        Err msg -> text msg
        Ok ma -> case castAes toA swv of
            Err msg -> text msg
            Ok sa -> h ma sa
  in case d of
    ADEnum opts -> castedView asWord8 WvWord8 <| enumEditor opts
    _ -> \_ _ -> text <| "Implement me: " ++ toString d

textViewer : String -> Maybe Attributee -> String -> Html a
textViewer re ma s = text s

refViewer : TypeName -> Maybe Attributee -> String -> Html a
refViewer tn ma tgt = text tgt

floatViewer : Bounds Float -> Maybe Attributee -> Float -> Html a
floatViewer b ma f = text <| toString f

enumViewer : List String -> Maybe Attributee -> Int -> Html a
enumViewer opts ma e = text <| case itemAtIndex e opts of
    Just v -> v
    Nothing -> "Enum index out of range"

enumEditor : List String -> Maybe Int -> AtomEditState Int -> Html Int
enumEditor opts me se = text "shuffle"
-- select
--     [onInput (Result.withDefault -1 << String.toInt)]
--     (List.indexedMap (\i o -> option [value (toString i), selected (i == e)] [text o]) opts)
