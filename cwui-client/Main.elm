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
import Form exposing (FormStore, formStoreEmpty, FormUiEvent, formClear, formUiUpdate, FormEvent(..), FormState(..), formState, UnboundFui(..), mapUfui, bindFui, castFormState)
import TupleViews exposing (viewConstTuple, viewConstNodeEdit)
import EditTypes exposing (NodeEdit(..), NodeEditEvent(..), NeChildrenT, asNeChildren, asNeConst, mapNee, NeChildState)

main = Html.program {
    init = init, update = update, subscriptions = subscriptions, view = view}

wsTarget : String
wsTarget = "ws://localhost:8004"

-- Model

type UiMode
  = UmEdit
  | UmView

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

mapNeeUf : (a -> b) -> UnboundFui a (NodeEditEvent a) -> UnboundFui b (NodeEditEvent b)
mapNeeUf f = mapUfui f (mapNee f)

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
    (Cannot, TupleDef d) -> case .interpLim d of
        ILUninterpolated -> viewCasted (castMaybe asConstDataNode) (viewConstTuple d) maybeNode
        _ -> text "Time series view not implemented"
    (_, TupleDef d) -> case .interpLim d of
        ILUninterpolated -> rcns NeConst asConstDataNode asNeConst <| viewConstNodeEdit d
        _ -> text "Time series edit not implemented"
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
