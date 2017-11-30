import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import JsonFudge exposing (serialiseBundle, parseBundle)
import ClTypes exposing (..)
import ClMsgTypes exposing (..)
import Futility exposing (mapAllFaily)
import WebSocket

import DepMap

main = Html.program {
    init = init, update = update, subscriptions = subscriptions, view = view}

wsTarget : String
wsTarget = "ws://localhost:8004"

-- Model

type alias TypeMap = DepMap.StringDeps
type alias NodeMap = Dict Path ClNode

-- FIXME: Drops site and attribution
type alias Model =
  { types : TypeMap
  , nodes : NodeMap
  , partialEntry : String
  , errors : List String
  }

init : (Model, Cmd Msg)
init = (Model DepMap.empty Dict.empty "somestring" [], Cmd.none)

clListToStrings : List ClValue -> Result String (List String)
clListToStrings =
  let
    rClString clv = case clv of
        (ClString s) -> Ok s
        _ -> Err "Value was not of string type"
  in
    mapAllFaily rClString

clListToInterps : List ClValue -> Result String (List Interpolation)
clListToInterps =
  let
    rInterp clv = case clv of
        (ClEnum e) -> case e of
            0 -> Ok IConstant
            1 -> Ok ILinear
            _ -> Err "Unrecognised interpolation enum value"
        _ -> Err "Interpolation ClValue not an enum"
  in
    mapAllFaily rInterp

clvToLiberty : ClValue -> Result String Liberty
clvToLiberty clv = case clv of
    (ClEnum e) -> case e of
        0 -> Ok Cannot
        1 -> Ok May
        2 -> Ok Must
        _ -> Err "Unrecognised value for liberty enum"
    _ -> Err "Liberty value not enum"

parseBaseType : Path -> ClNode -> Result String ClType
parseBaseType tp n = case tp of
    "/api/types/base/tuple" -> case Dict.get zt (.values n) of
        (Just [ClString doc, ClList vNames, ClList vAtomTypes, ClList vInterps]) ->
            Result.map3
                (\ns ats itps -> ClTuple {
                    doc = doc, names = ns, atomTypes = ats,
                    interpolations = itps})
                (clListToStrings vNames)
                (clListToStrings vAtomTypes)
                (clListToInterps vInterps)
        (Just _) -> Err "Invalid value types for type info"
        Nothing -> Err "No value at t=0 in type info"
    "/api/types/base/struct" -> case Dict.get zt (.values n) of
        (Just [ClString doc, ClList vChildNames, ClList vChildTypes, ClList vChildLibs]) ->
            Result.map3
                (\cn ct cl -> ClStruct {
                    doc = doc, childNames = cn, childTypes = ct,
                    childLiberties = cl})
                (clListToStrings vChildNames)
                (clListToStrings vChildTypes)
                (mapAllFaily clvToLiberty vChildLibs)
        (Just _) -> Err "Invalid value types for struct def"
        Nothing -> Err "No value at t=0 for struct def"
    "/api/types/base/array" -> case Dict.get zt (.values n) of
        (Just [ClString doc, ClString childType, ClEnum vLib]) -> Result.map
            (\l -> ClArray {doc = doc, childType = childType, childLiberty = l})
            (clvToLiberty (ClEnum vLib))
        (Just _) -> Err "Invalid value types for array def"
        Nothing -> Err "No value at t=0 for array def"
    _ -> Err "Not a base type path"

clTypeAt : Path -> NodeMap -> TypeMap -> Result String ClType
clTypeAt p nm tm = case DepMap.getDependency p tm of
    Nothing -> Err "Type not loaded"
    (Just tp) -> case Dict.get p nm of
        Nothing -> Err "Type of type missing"
        (Just n) -> parseBaseType tp n

clTypeOf : Path -> NodeMap -> TypeMap -> Result String ClType
clTypeOf p nm tm = case DepMap.getDependency p tm of
    Nothing -> Err "Missing from the type map"
    (Just tp) -> clTypeAt tp nm tm

-- Update

type InterfaceEvent = UpPartial String | IfSub String

interfaceUpdate : InterfaceEvent -> Model -> (Model, Cmd Msg)
interfaceUpdate ie model = case ie of
    (UpPartial s) -> ({model | partialEntry = s}, Cmd.none)
    (IfSub s) -> ({model | nodes = Dict.insert s emptyNode (.nodes model)}, WebSocket.send wsTarget (serialiseBundle (RequestBundle [MsgSub s] [])))

handleDataUpdateMsg : DataUpdateMsg -> ClNode -> ClNode
handleDataUpdateMsg dum n = let nv = .values n in case dum of
    (MsgAdd {msgTime, msgArgs}) -> {n | values = Dict.insert msgTime msgArgs nv}
    (MsgSet {msgTime, msgArgs}) -> {n | values = Dict.insert msgTime msgArgs nv}
    (MsgRemove {msgTime}) -> {n | values = Dict.remove msgTime nv}
    (MsgClear {}) -> n  -- FIXME: does nothing because single site
    (MsgSetChildren {}) -> n  -- FIXME: don't have anywhere to put this in the model ATM

handleTreeUpdateMsg : TreeUpdateMsg -> TypeMap -> TypeMap
handleTreeUpdateMsg tum tm = case tum of
    (MsgAssignType p tp) -> DepMap.addDependency p tp tm
    (MsgDelete p) -> DepMap.removeDependency p tm

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

handleErrorMsg : ErrorMsg -> NodeMap -> NodeMap
handleErrorMsg (ErrorMsg p msg) nm = updateNode (\n -> {n | errors = msg :: .errors n}) p nm

handleUpdateBundle : UpdateBundle -> (NodeMap, TypeMap) -> (NodeMap, TypeMap)
handleUpdateBundle (UpdateBundle errs updates) maps =
  let
    (updatedNodes, updatedTypes) = List.foldl handleUpdateMsg maps updates
    nodesWithErrs = List.foldl handleErrorMsg updatedNodes errs
  in
    (nodesWithErrs, updatedTypes)
    -- FIXME: doesn't sort types

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
    (GlobalError s) -> ({model | errors = s :: .errors model}, Cmd.none)

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model = WebSocket.listen wsTarget eventFromNetwork

eventFromNetwork : String -> Msg
eventFromNetwork s = case parseBundle s of
    (Ok b) -> NetworkEvent b
    (Err e) -> GlobalError e

-- View

view : Model -> Html Msg
view {errors, types, nodes, partialEntry} = Html.map InterfaceEvent (
    div [] [viewErrors errors, subControl partialEntry, viewPaths types nodes])

viewErrors : List String -> Html InterfaceEvent
viewErrors errs = ul [] (List.map (\s -> li [] [text s]) errs)

subControl : String -> Html InterfaceEvent
subControl path = div []
  [ input [type_ "text", placeholder "Path", onInput (UpPartial)] []
  , button [onClick (IfSub path)] [text "sub"]
  ]

viewPaths : TypeMap -> NodeMap -> Html InterfaceEvent
viewPaths types nodes = div [] (
    List.map (\(p, n) -> div [] [ text p , text (toString (clTypeOf p nodes types)) , viewNode n]) (Dict.toList nodes))

viewNode : ClNode -> Html InterfaceEvent
viewNode node = table [] (
    List.map
    (\(i, vs) -> tr [] (td [] [text (toString i)] :: List.map (\v -> td [] [text (toString v)]) vs))
    (Dict.toList (.values node)))
