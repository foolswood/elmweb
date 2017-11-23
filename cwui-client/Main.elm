import Dict exposing (..)
import Set exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Json.Encode as JE
import WebSocket

main = Html.program {
    init = init, update = update, subscriptions = subscriptions, view = view}

-- Model

type alias Path = String
type alias ChildName = String
type alias Time = Int
type alias Attributee = String
type alias Site = String

type Liberty
  = Cannot
  | May
  | Must

type Interpolation
  = IConstant
  | ILinear

type alias ClAtomType = String

type ClType
  = ClTuple {doc : String, names : List String, atomTypes : List ClAtomType, interpolations : Set Interpolation}
  | ClStruct {doc : String, childNames: List ChildName, childTypes : List Path, childLiberties : List Liberty}
  | ClArray {doc : String, childType : Path, childLiberty : Liberty}

type ClValue
  = ClTime Time
  | ClEnum Int
  | ClWord32 Int
  | ClWord64 Int
  | ClInt32 Int
  | ClInt64 Int
  | ClFloat Float
  | ClDouble Float
  | ClString String
  | ClList (List ClValue)

type alias ClSeries = Dict Time (List ClValue)

-- FIXME: nowhere to put tree structure or errors!
type alias ClNode =
  { typeInfo : Maybe ClType
  , values : ClSeries
  , pending : ClSeries
  }

emptyNode : ClNode
emptyNode = ClNode Nothing Dict.empty Dict.empty

type alias TypeMap = Dict Path Path
type alias NodeMap = Dict Path ClNode

-- FIXME: Drops site and attribution
type alias Model =
  { types : TypeMap
  , nodes : NodeMap
  , partialEntry : String
  }

init : (Model, Cmd Msg)
init = (Model Dict.empty Dict.empty "somestring", Cmd.none)

-- Update

type InterfaceEvent = UpPartial String | IfSub String

interfaceUpdate : InterfaceEvent -> Model -> (Model, Cmd Msg)
interfaceUpdate ie model = case ie of
    (UpPartial s) -> ({model | partialEntry = s}, Cmd.none)
    (IfSub s) -> ({model | nodes = Dict.insert s emptyNode (.nodes model)}, WebSocket.send "ws://echo.websocket.org" (serialiseBundle (RequestBundle [MsgSub s] [])))

type SubMsg
  = MsgSub Path
  | MsgUnsub Path

type RequestBundle = RequestBundle (List SubMsg) (List DataUpdateMsg)

type DataUpdateMsg
  = MsgAdd
      { msgPath : Path
      , msgTime : Time
      , msgArgs : (List ClValue)
      , msgInterpolation : Interpolation
      , msgAttributee : (Maybe Attributee)
      , msgSite : (Maybe Site)
      }
  | MsgSet
      { msgPath : Path
      , msgTime : Time
      , msgArgs : List ClValue
      , msgInterpolation : Interpolation
      , msgAttributee : Maybe Attributee
      , msgSite : Maybe Site
      }
  | MsgRemove
      { msgPath : Path
      , msgTime : Time
      , msgAttributee : Maybe Attributee
      , msgSite : Maybe Site
      }
  | MsgClear
      { msgPath : Path
      , msgTime : Time
      , msgAttributee : Maybe Attributee
      , msgSite : Maybe Site
      }
  | MsgSetChildren
      { msgPath : Path
      , msgChildren : List ChildName
      , msgAttributee : Maybe Attributee
      }

handleDataUpdateMsg : DataUpdateMsg -> ClNode -> ClNode
handleDataUpdateMsg dum n = let nv = .values n in case dum of
    (MsgAdd {msgTime, msgArgs}) -> {n | values = Dict.insert msgTime msgArgs nv}
    (MsgSet {msgTime, msgArgs}) -> {n | values = Dict.insert msgTime msgArgs nv}
    (MsgRemove {msgTime}) -> {n | values = Dict.remove msgTime nv}
    (MsgClear {}) -> n  -- FIXME: does nothing because single site
    (MsgSetChildren {}) -> n  -- FIXME: don't have anywhere to put this in the model ATM

type ErrorMsg = ErrorMsg Path String

type TreeUpdateMsg
  = MsgAssignType Path Path
  | MsgDelete Path

handleTreeUpdateMsg : TreeUpdateMsg -> TypeMap -> TypeMap
handleTreeUpdateMsg tum tm = case tum of
    (MsgAssignType p tp) -> Dict.insert p tp tm
    (MsgDelete p) -> Dict.remove p tm

type UpdateMsg = TreeUpdateMsg TreeUpdateMsg | DataUpdateMsg DataUpdateMsg

updateNode : (ClNode -> ClNode) -> Path -> NodeMap -> NodeMap
updateNode t p m =
  let
    wt mn = Just (t (Maybe.withDefault emptyNode mn))
  in
    Dict.update p wt m

dumPath : DataUpdateMsg -> Path
dumPath dum = case dum of
    (MsgAdd {msgPath}) -> msgPath
    (MsgSet {msgPath}) -> msgPath
    (MsgRemove {msgPath}) -> msgPath
    (MsgClear {msgPath}) -> msgPath
    (MsgSetChildren {msgPath}) -> msgPath

handleUpdateMsg : UpdateMsg -> (NodeMap, TypeMap) -> (NodeMap, TypeMap)
handleUpdateMsg um (nodes, types) = case um of
    (TreeUpdateMsg tum) -> (nodes, handleTreeUpdateMsg tum types)
    (DataUpdateMsg dum) -> (updateNode (handleDataUpdateMsg dum) (dumPath dum) nodes, types)

type UpdateBundle = UpdateBundle (List ErrorMsg) (List UpdateMsg)

handleUpdateBundle : UpdateBundle -> (NodeMap, TypeMap) -> (NodeMap, TypeMap)
handleUpdateBundle (UpdateBundle errs updates) maps =
    List.foldl handleUpdateMsg maps updates
    -- FIXME: Ignores errors and doesn't sort types

type Msg
  = GlobalError String
  | InterfaceEvent InterfaceEvent
  | NetworkEvent UpdateBundle

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    (NetworkEvent b) ->
      let
        (newNodes, newTypes) = handleUpdateBundle b (.nodes model, .types model)
      in
        ({model | nodes = newNodes, types = newTypes}, Cmd.none)
    (InterfaceEvent ie) -> interfaceUpdate ie model
    (GlobalError s) -> (model, Cmd.none)  -- FIXME: ignoring fatal errors!

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model = WebSocket.listen "ws://echo.websocket.org" eventFromNetwork

eventFromNetwork : String -> Msg
eventFromNetwork s = case parseBundle s of
    (Ok b) -> NetworkEvent b
    (Err e) -> GlobalError e

-- View

view : Model -> Html Msg
view {nodes, partialEntry} = Html.map InterfaceEvent (div [] [subControl partialEntry, viewPaths nodes])

subControl : String -> Html InterfaceEvent
subControl path = div []
  [ input [type_ "text", placeholder "Path", onInput (UpPartial)] []
  , button [onClick (IfSub path)] [text "cons"]
  ]

viewPaths : NodeMap -> Html InterfaceEvent
viewPaths nodes = div [] (
    List.map (\(p, n) -> div [] [ text p , viewNode n]) (Dict.toList nodes))

viewNode : ClNode -> Html InterfaceEvent
viewNode node = table [] (
    List.map
    (\(i, vs) -> tr [] (td [] [text (toString i)] :: List.map (\v -> td [] [text (toString v)]) vs))
    (Dict.toList (.values node)))

-- Json serialisation fudge

subMsgToJsonValue : SubMsg -> JE.Value
subMsgToJsonValue sm = JE.list (List.map JE.string (case sm of
    (MsgSub p) -> ["s", p]
    (MsgUnsub p) -> ["u", p]))

serialiseBundle : RequestBundle -> String
serialiseBundle (RequestBundle subs dums) = JE.encode 2 (JE.list (List.map subMsgToJsonValue subs))

parseBundle : String -> Result String UpdateBundle
parseBundle s = Ok (
    UpdateBundle [] [DataUpdateMsg (MsgAdd {
        msgPath = s, msgTime = 0, msgArgs = [ClString "blah"], msgInterpolation = IConstant,
        msgAttributee = Nothing, msgSite = Nothing})])
