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
import ClMsgTypes exposing (FromRelayClientBundle, ToRelayClientBundle(..), SubMsg(MsgSub))
import Futility exposing (..)
import RelayState exposing (..)
import MonoTime
import Layout exposing (Layout(..), LayoutPath, setLeafBinding, viewEditLayout, viewLayout, layoutRequires)
import Form exposing (FormStore, formStoreEmpty, FormUiEvent(..), formClear, formUiUpdate, FormEvent(..), FormState(..), formState)

main = Html.program {
    init = init, update = update, subscriptions = subscriptions, view = view}

wsTarget : String
wsTarget = "ws://localhost:8004"

-- Model

type UiMode
  = UmEdit
  | UmView

type alias NeConstT = List (Maybe WireValue)

type NodeEdit
  = NeConst NeConstT

asNeConst : NodeEdit -> Result String NeConstT
asNeConst edit = case edit of
    NeConst e -> Ok e

type alias Model =
  -- Global:
  { globalErrs : List String
  , viewMode : UiMode
  -- Layout:
  , layout : Layout Path
  , layoutFs : FormStore LayoutPath Path
  -- Data:
  , subs : Set Path
  , types : TypeMap
  , tyAssns : TypeAssignMap
  , nodes : NodeMap
  , nodeFs : FormStore Path NodeEdit
  }

init : (Model, Cmd Msg)
init =
  let
    initialLayout = LayoutContainer [LayoutLeaf "/relay/self"]
    initialSubs = layoutRequires initialLayout
    initialModel =
      { globalErrs = []
      , viewMode = UmEdit
      , layout = LayoutContainer [LayoutLeaf "/relay/self"]
      , layoutFs = formStoreEmpty
      , subs = initialSubs
      , types = Dict.empty
      , tyAssns = Dict.empty
      , nodes = Dict.empty
      , nodeFs = formStoreEmpty
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
  = GlobalError String
  | SwapViewMode
  | NetworkEvent FromRelayClientBundle
  | TimeStamped (Time -> Cmd Msg) Time.Time
  | LayoutUiEvent (FormUiEvent LayoutPath Path)
  | NodeUiEvent (FormUiEvent Path NodeEdit)

timeStamped : (Time -> Cmd Msg) -> Cmd Msg
timeStamped c = Task.perform (TimeStamped c) MonoTime.now

feHandler : Model -> FormEvent k v -> (k -> v -> (Model, Cmd Msg)) -> (Model, Cmd Msg)
feHandler m fe submitHandler = case fe of
    FeNoop -> (m, Cmd.none)
    FeError msg -> update (GlobalError msg) m
    FeSubmit k v -> submitHandler k v

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    GlobalError s -> ({model | globalErrs = s :: .globalErrs model}, Cmd.none)
    NetworkEvent b ->
      let
        (newNodes, newAssns, newTypes, globalErrs) = handleFromRelayBundle b (.nodes model) (.tyAssns model) (.types model)
      in
        ({model | nodes = newNodes, tyAssns = newAssns, types = newTypes, globalErrs = globalErrs ++ .globalErrs model}, Cmd.none)
    TimeStamped c t -> (model, c (fromFloat t))
    SwapViewMode -> case .viewMode model of
        UmEdit -> ({model | viewMode = UmView}, Cmd.none)
        UmView -> ({model | viewMode = UmEdit}, Cmd.none)
    LayoutUiEvent fue ->
      let
        (newLayoutFs, fe) = formUiUpdate fue <| .layoutFs model
        newM = {model | layoutFs = newLayoutFs}
      in feHandler newM fe <| \lp p -> case setLeafBinding lp p <| .layout model of
        Err msg -> update (GlobalError msg) newM
        Ok newLayout ->
          let
            editProcessedFs = formClear lp <| .layoutFs newM
            subs = layoutRequires newLayout
          in ({newM | layout = newLayout, layoutFs = editProcessedFs, subs = subs}, subDiffToCmd (.subs newM) subs)
    NodeUiEvent fue ->
      let
        (newNodeFs, fe) = formUiUpdate fue <| .nodeFs model
        newM = {model | nodeFs = newNodeFs}
      in feHandler newM fe <| \p n -> update (GlobalError "Node edit not implemented") newM

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model = WebSocket.listen wsTarget eventFromNetwork

eventFromNetwork : String -> Msg
eventFromNetwork s = case parseBundle s of
    (Ok b) -> NetworkEvent b
    (Err e) -> GlobalError e

-- View

view : Model -> Html Msg
view m = div []
  [ viewErrors <| .globalErrs m
  , button [onClick SwapViewMode] [text "switcheroo"]
  , case .viewMode m of
    UmEdit -> Html.map LayoutUiEvent <| viewEditLayout pathEditView (.layoutFs m) (.layout m)
    UmView -> Html.map NodeUiEvent <| viewLayout (viewPath (.nodeFs m) (.types m) (.tyAssns m) (.nodes m)) (.layout m)
  ]

viewErrors : List String -> Html a
viewErrors errs = ul [] (List.map (\s -> li [] [text s]) errs)

pathEditView : LayoutPath -> Path -> FormState Path -> Html (FormUiEvent LayoutPath Path)
pathEditView lp p fs = case fs of
    FsViewing -> span [onClick <| FuePartial lp p] [text p]
    FsEditing partial -> Html.span []
      [ button [onClick <| FueSubmit lp] [text <| "Replace " ++ p]
      , input [value partial, type_ "text", onInput <| FuePartial lp] []
      ]
    FsPending pending -> span [onClick <| FuePartial lp pending] [text <| p ++ " -> " ++ pending]

-- FIXME: Other one is a wrapper around this really?
type UnboundFui v
  = UfPartial v
  | UfSubmit

mapUfui : (a -> b) -> UnboundFui a -> UnboundFui b
mapUfui f e = case e of
    UfPartial a -> UfPartial <| f a
    UfSubmit -> UfSubmit

bindFui : k -> UnboundFui v -> FormUiEvent k v
bindFui k e = case e of
    UfPartial v -> FuePartial k v
    UfSubmit -> FueSubmit k

viewPath : FormStore Path NodeEdit -> TypeMap -> TypeAssignMap -> NodeMap -> Path -> Html (FormUiEvent Path NodeEdit)
viewPath fs types tyAssns nodes p = case Dict.get p tyAssns of
    Nothing -> text <| "Loading type info: " ++ p
    Just (tn, lib) -> case Dict.get tn types of
        Nothing -> text <| "Type missing from map: " ++ toString tn
        Just ty -> case Dict.get p nodes of
            Nothing -> text <| "No data for: " ++ p
            Just n -> Html.map (bindFui p) <| viewNode lib ty n <| formState p fs

castFormState : (a -> Result String b) -> FormState a -> Result String (FormState b)
castFormState c f = case f of
    FsViewing -> Ok FsViewing
    FsEditing a -> Result.map FsEditing <| c a
    FsPending a -> Result.map FsPending <| c a

viewNode : Liberty -> Definition -> Node -> FormState NodeEdit -> Html (UnboundFui NodeEdit)
viewNode lib def node formState = case (lib, def, node) of
    (Cannot, TupleDef d, ConstDataNode n) -> viewConstTuple (List.map Tuple.second <| .types d) (Just <| .values n)
    (_, TupleDef d, ConstDataNode n) -> case castFormState asNeConst formState of
        Ok s -> Html.map (mapUfui NeConst) <| viewConstNodeEdit d n s
        Err msg -> text msg
    (_, StructDef d, ContainerNode n) -> text "Struct edit not implemented"
    (_, ArrayDef d, ContainerNode n) -> text "Array edit not implemented"
    _ -> text "Def/node type mismatch"

viewConstTuple : List AtomDef -> Maybe ConstData -> Html a
viewConstTuple defs mv = case mv of
    Nothing -> text "Awaiting data"
    Just (ma, vs) -> span [] <| List.map2 (viewAtom ma) defs vs

viewAtom : Maybe Attributee -> AtomDef -> WireValue -> Html a
viewAtom ma def wv =
  let
    castedView : (WireValue -> Result String b) -> (Maybe Attributee -> b -> Html a) -> Html a
    castedView c h = case c wv of
        Ok a -> h ma a
        Err msg -> text msg
  in case def of
    ADEnum opts -> castedView asWord8 <| enumViewer opts
    ADString re -> castedView asString <| textViewer re
    ADRef ty -> castedView asString <| refViewer ty
    _ -> text <| "View not implemented: " ++ toString def

-- FIXME: Dodgy Just due to maybe not coming in here:
viewConstNodeEdit : TupleDefinition -> ConstDataNodeT -> FormState NeConstT -> Html (UnboundFui NeConstT)
viewConstNodeEdit d n s = viewConstTupleEdit (List.map Tuple.second <| .types d) (Just <| Tuple.second <| .values n) s

-- FIXME: Move to Form?
type AtomEditState a
  = AesViewing
  | AesUnfilled
  | AesEditing a

viewConstTupleEdit
   : List AtomDef -> Maybe (List WireValue) -> FormState NeConstT
   -> Html (UnboundFui NeConstT)
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
        FsPending mevs -> (mevs, List.map toAes mevs)
    asPartial idx wv = UfPartial <| Result.withDefault editBase <| replaceIdx idx (Just wv) editBase
    atomEditor idx def mwv aes = Html.map (asPartial idx) <| viewAtomEdit def mwv aes
    atomEditors = List.map4 atomEditor (List.range 0 nDefs) defs current atomEditStates
    filledFields = List.filterMap identity editBase
    content = if List.length filledFields == List.length defs
      then button [onClick UfSubmit] [text "Apply"] :: atomEditors
      else atomEditors
  in span [] content

viewAtomEdit : AtomDef -> Maybe WireValue -> AtomEditState WireValue -> Html WireValue
viewAtomEdit d =
  let
    castAes : (a -> Result String b) -> AtomEditState a -> Result String (AtomEditState b)
    castAes c s = case s of
        AesViewing -> Ok AesViewing
        AesUnfilled -> Ok AesUnfilled
        AesEditing v -> Result.map AesEditing <| c v
    castMaybe : (a -> Result String b) -> Maybe a -> Result String (Maybe b)
    castMaybe c m = case m of
        Nothing -> Ok Nothing
        Just v -> Result.map Just <| c v
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

enumViewer : List String -> Maybe Attributee -> Int -> Html a
enumViewer opts ma e = text <| case itemAtIndex e opts of
    Just v -> v
    Nothing -> "Enum index out of range"

enumEditor : List String -> Maybe Int -> AtomEditState Int -> Html Int
enumEditor opts me se = text "shuffle"
-- select
--     [onInput (Result.withDefault -1 << String.toInt)]
--     (List.indexedMap (\i o -> option [value (toString i), selected (i == e)] [text o]) opts)
