import Dict exposing (..)
import Set exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Json.Encode as JE
import Json.Decode as JD
import WebSocket

import DepMap

main = Html.program {
    init = init, update = update, subscriptions = subscriptions, view = view}

wsTarget : String
wsTarget = "ws://localhost:8004"

-- Model

type alias Path = String
type alias ChildName = String
type alias Time = (Int, Int)
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
  = ClTuple {
        doc : String,
        names : List String,
        atomTypes : List ClAtomType,
        interpolations : List Interpolation} -- Can't define own types that go in a set (because comparable)
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

-- FIXME: nowhere to put tree structure!
type alias ClNode =
  { errors : List String
  , values : ClSeries
  , pending : ClSeries
  }

emptyNode : ClNode
emptyNode = ClNode [] Dict.empty Dict.empty

zt : Time
zt = (0, 0)

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

-- Equivalent to mapM for Result:
mapAllFaily : (a -> Result x b) -> List a -> Result x (List b)
mapAllFaily act vs = List.foldl
    (\v -> Result.andThen
        (\acc -> Result.map (\b -> b :: acc) (act v))
        )
    (Ok [])
    vs

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
    (MsgAssignType p tp) -> DepMap.addDependency p tp tm
    (MsgDelete p) -> DepMap.removeDependency p tm

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

-- Json serialisation fudge

subMsgToJsonValue : SubMsg -> JE.Value
subMsgToJsonValue sm = JE.list (List.map JE.string (case sm of
    (MsgSub p) -> ["S", p]
    (MsgUnsub p) -> ["U", p]))

serialiseBundle : RequestBundle -> String
serialiseBundle (RequestBundle subs dums) = JE.encode 2 (JE.object [("subs", (JE.list (List.map subMsgToJsonValue subs))), ("dums", JE.list [])])

decodePath : JD.Decoder String
decodePath = JD.string

decodeErrMsg : JD.Decoder ErrorMsg
decodeErrMsg = JD.map2 ErrorMsg (JD.field "path" decodePath) (JD.field "msg" JD.string)

decodeTime : JD.Decoder Time
decodeTime = JD.map2 (\a b -> (a, b)) (JD.index 0 JD.int) (JD.index 1 JD.int)

decodeClValue : JD.Decoder ClValue
decodeClValue =
  let
    decoderForClValueTag t = case t of
        "t" -> JD.map ClTime decodeTime
        "e" -> JD.map ClEnum JD.int
        "u" -> JD.map ClWord32 JD.int
        "U" -> JD.map ClWord64 JD.int
        "i" -> JD.map ClInt32 JD.int
        "I" -> JD.map ClInt64 JD.int
        "d" -> JD.map ClFloat JD.float
        "D" -> JD.map ClDouble JD.float
        "s" -> JD.map ClString JD.string
        "l" -> JD.map ClList (JD.list decodeClValue)
        _ -> JD.fail "unrecognised tag"
  in
    JD.andThen (JD.index 1 << decoderForClValueTag) (JD.index 0 JD.string)

decodeInterpolation : JD.Decoder Interpolation
decodeInterpolation =
  let
    decoderForTag t = case t of
        "C" -> JD.succeed IConstant
        "L" -> JD.succeed ILinear
        _ -> JD.fail "unrecognised interpolation tag"
  in
    JD.andThen decoderForTag (JD.index 0 JD.string)

decodeMsg : JD.Decoder UpdateMsg
decodeMsg =
  let
    decodePathField = JD.field "path" decodePath
    decodeTimeField = JD.field "time" decodeTime
    decodeArgsField = JD.field "args" (JD.list decodeClValue)
    decodeInterpolationField = JD.field "interpolation" decodeInterpolation
    decoderForTag t = case t of
        "A" -> JD.map2
            (\p tp -> TreeUpdateMsg (MsgAssignType p tp))
            decodePathField (JD.field "type" decodePath)
        "C" -> JD.map2
            (\p ns -> DataUpdateMsg (MsgSetChildren {
                msgPath=p, msgChildren=ns, msgAttributee=Nothing}))
            decodePathField (JD.field "children" (JD.list JD.string))
        "a" -> JD.map4
            (\p t a i -> DataUpdateMsg (MsgAdd {
                msgPath=p, msgTime=t, msgArgs=a, msgInterpolation=i,
                msgAttributee=Nothing, msgSite=Nothing}))
            decodePathField decodeTimeField decodeArgsField decodeInterpolationField
        "s" -> JD.map4
            (\p t a i -> DataUpdateMsg (MsgSet {
                msgPath=p, msgTime=t, msgArgs=a, msgInterpolation=i,
                msgAttributee=Nothing, msgSite=Nothing}))
            decodePathField decodeTimeField decodeArgsField decodeInterpolationField
        _ -> JD.fail "unrecognised msg tag"
  in
    JD.andThen (JD.index 1 << decoderForTag) (JD.index 0 JD.string)

parseBundle : String -> Result String UpdateBundle
parseBundle = JD.decodeString (JD.map2 UpdateBundle (JD.field "errs" (JD.list decodeErrMsg)) (JD.field "ups" (JD.list decodeMsg)))
