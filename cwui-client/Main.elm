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
import Form exposing (FormStore, formStoreEmpty, FormUiEvent(..), formClear, formUiUpdate, FormEvent(..), FormState(..))

main = Html.program {
    init = init, update = update, subscriptions = subscriptions, view = view}

wsTarget : String
wsTarget = "ws://localhost:8004"

-- Model

type UiMode
  = UmEdit
  | UmView

type NodeEdit
  = NeConst List WireValue

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
    UmView -> Html.map NodeUiEvent <| viewLayout (viewPath (.types m) (.tyAssns m) (.nodes m)) (.layout m)
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

viewPath : TypeMap -> TypeAssignMap -> NodeMap -> Path -> Html (FormUiEvent Path NodeEdit)
viewPath types tyAssns nodes p = case Dict.get p tyAssns of
    Nothing -> text <| "Loading type info: " ++ p
    Just (tn, lib) -> case Dict.get tn types of
        Nothing -> text <| "Type missing from map: " ++ toString tn
        Just ty -> case Dict.get p nodes of
            Nothing -> text <| "No data for: " ++ p
            Just n -> Html.map (FuePartial p) <| viewNode lib ty n

viewNode : Liberty -> Definition -> Node -> Html NodeEdit
viewNode lib def node = text <| toString (lib, def, node)

enumEditor : List String -> Int -> Html Int
enumEditor opts e = select
    [onInput (Result.withDefault -1 << String.toInt)]
    (List.indexedMap (\i o -> option [value (toString i), selected (i == e)] [text o]) opts)
