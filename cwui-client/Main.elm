import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import WebSocket

import JsonFudge exposing (serialiseBundle, parseBundle)
import ClTypes exposing (..)
import ClMsgTypes exposing (..)
import ClSpecParser exposing (parseBaseType)
import DepMap
import Futility exposing (..)
import PathManipulation exposing (splitBasename)

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

clLibertyOf : Path -> NodeMap -> TypeMap -> Result String Liberty
clLibertyOf p nm tm =
  let
    rTypeP pp = Result.fromMaybe (String.append "Parent type not in map: " pp) (DepMap.getDependency pp tm)
    rTypeAt tp = clTypeAt tp nm tm
    rChildLib cn tn = case tn of
        (ClStruct {childNames, childLiberties}) -> Result.fromMaybe "No matching child" (
            Maybe.map snd (firstMatching (\(icn, _) -> cn == icn) (zip childNames childLiberties)))
        (ClArray {childLiberty}) -> Ok childLiberty
        ClTuple _ -> Err "Parent was a tuple"
  in
    case splitBasename p of
        Nothing -> Ok Cannot  -- Root
        Just (pp, cn) -> Result.andThen (rChildLib cn) (Result.andThen rTypeAt (rTypeP pp))

-- Update
type InterfaceEvent = UpPartial String | IfSub Path | TimePointEdit Path Time (List ClValue)

sendBundle : RequestBundle -> Cmd Msg
sendBundle = WebSocket.send wsTarget << serialiseBundle

subToCmd : List Path -> Cmd Msg
subToCmd ps = case ps of
    [] -> Cmd.none
    _ -> sendBundle (RequestBundle (List.map MsgSub ps) [])

interfaceUpdate : InterfaceEvent -> Model -> (Model, Cmd Msg)
interfaceUpdate ie model = case ie of
    (UpPartial s) -> ({model | partialEntry = s}, Cmd.none)
    (IfSub s) -> ({model | nodes = Dict.insert s emptyNode (.nodes model)}, subToCmd [s])
    -- FIXME: always sends sets, no pending
    (TimePointEdit p t vs) -> (model, sendBundle (RequestBundle [] [MsgSet {
        msgPath = p, msgTime = t, msgArgs = vs, msgInterpolation = IConstant,
        msgAttributee = Nothing, msgSite = Nothing}]))

tupleNodeUpdate : (ClSeries -> ClSeries) -> ClNode -> ClNode
tupleNodeUpdate op n = case .body n of
    TupleNode tn -> {n | body = TupleNode {tn | values = op (.values tn)}}
    _ -> tupleNodeUpdate op {n | body = emptyTupleNode}

containerNodeUpdate : List ChildName -> ClNode -> ClNode
containerNodeUpdate cns n = case .body n of
    ContainerNode cn -> {n | body = ContainerNode {cn | children = cns}}
    _ -> containerNodeUpdate cns {n | body = emptyContainerNode}

handleDataUpdateMsg : DataUpdateMsg -> ClNode -> ClNode
handleDataUpdateMsg dum = case dum of
    (MsgAdd {msgTime, msgArgs}) -> tupleNodeUpdate (Dict.insert msgTime msgArgs)
    (MsgSet {msgTime, msgArgs}) -> tupleNodeUpdate (Dict.insert msgTime msgArgs)
    (MsgRemove {msgTime}) -> tupleNodeUpdate (Dict.remove msgTime)
    (MsgClear {}) -> \n -> n  -- FIXME: does nothing because single site
    (MsgSetChildren {msgChildren}) -> containerNodeUpdate msgChildren

handleTreeUpdateMsg : TreeUpdateMsg -> TypeMap -> (TypeMap, Maybe Path)
handleTreeUpdateMsg tum tm = case tum of
    (MsgAssignType p tp) -> (DepMap.addDependency p tp tm, Just tp)
    (MsgDelete p) -> (DepMap.removeDependency p tm, Nothing)

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

handleUpdateMsg : UpdateMsg -> (NodeMap, TypeMap, List Path) -> (NodeMap, TypeMap, List Path)
handleUpdateMsg um (nodes, types, ntp) = case um of
    (TreeUpdateMsg tum) ->
      let
        (nt, mp) = handleTreeUpdateMsg tum types
        entp = case mp of
            Nothing -> ntp
            Just p -> p :: ntp
      in
        (nodes, nt, entp)
    (DataUpdateMsg dum) -> (updateNode (handleDataUpdateMsg dum) (dumPath dum) nodes, types, ntp)

handleErrorMsg : ErrorMsg -> NodeMap -> NodeMap
handleErrorMsg (ErrorMsg p msg) nm = updateNode (\n -> {n | errors = msg :: .errors n}) p nm

handleUpdateBundle : UpdateBundle -> (NodeMap, TypeMap) -> (NodeMap, TypeMap, List Path)
handleUpdateBundle (UpdateBundle errs updates) (nodes, types) =
  let
    (updatedNodes, updatedTypes, newTypePaths) = List.foldl handleUpdateMsg (nodes, types, []) updates
    nodesWithErrs = List.foldl handleErrorMsg updatedNodes errs
    unsubbedTypes = List.filter (not << flip Dict.member updatedNodes) newTypePaths
  in
    (nodesWithErrs, updatedTypes, unsubbedTypes)

type Msg
  = GlobalError String
  | InterfaceEvent InterfaceEvent
  | NetworkEvent UpdateBundle

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    (NetworkEvent b) ->
      let
        (newNodes, newTypes, unsubbedTypes) = handleUpdateBundle b (.nodes model, .types model)
      in
        ({model | nodes = newNodes, types = newTypes}, subToCmd unsubbedTypes)
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

viewErrors : List String -> Html a
viewErrors errs = ul [] (List.map (\s -> li [] [text s]) errs)

subControl : String -> Html InterfaceEvent
subControl path = div []
  [ input [type_ "text", placeholder "Path", onInput (UpPartial)] []
  , button [onClick (IfSub path)] [text "sub"]
  ]

viewPaths : TypeMap -> NodeMap -> Html InterfaceEvent
viewPaths types nodes = div [] (List.map
    (\(p, n) -> div [] [
        h2 [] [text p]
      , Html.map (uncurry (TimePointEdit p)) (viewNode (clLibertyOf p nodes types) (clTypeOf p nodes types) n)])
    (Dict.toList nodes))

viewNode : Result String Liberty -> Result String ClType -> ClNode -> Html TupleEdit
viewNode rLiberty rType node =
  let
    (liberty, allErrors) = case rLiberty of
        Err s -> (Cannot, s :: .errors node)
        Ok l -> (l, .errors node)
    nv = case rType of
        Err s -> text s
        Ok clType -> case .body node of
            TupleNode tn -> case clType of
                ClTuple tti -> viewTuple liberty tti tn
                _ -> text "Tuple definition non-tuple node"
            ContainerNode cn -> case clType of
                ClTuple _ -> text "Tuple type for container node"
                ClStruct {doc} -> viewContainer doc cn
                ClArray {doc} -> viewContainer doc cn
            UnpopulatedNode -> text "Unpopulated node of known type"
  in
    div [] [viewErrors allErrors, nv]

viewContainer : String -> ClContainerNode -> Html TupleEdit
viewContainer doc {children} = div [] (text doc :: List.map (\c -> text c) children)

type alias AtomEdit = ClValue
type alias TupleEdit = (Time, List AtomEdit)

viewTuple : Liberty -> ClTupleType -> ClTupleNode -> Html TupleEdit
viewTuple liberty {names, atomTypes} node =
  let
    atomRenderer = case liberty of
        Cannot -> atomViewer
        May -> atomEditor
        Must -> atomEditor
    updateItem idx vs nv = List.indexedMap (\i v -> if i == idx then nv else v) vs
    tupleRenderer : List ClValue -> List (Html (List AtomEdit))
    tupleRenderer vs = List.indexedMap
        (\idx (v, at) -> td [] [Html.map (updateItem idx vs) (atomRenderer at v)])
        (zip vs atomTypes)
    tpRenderer t vs = tr [] (td [] [text (toString t)] :: List.map (Html.map (\tup -> (t, tup))) (tupleRenderer vs))
    tView = tr [] (th [] [text "Time"] :: List.map (\(n, at) -> th [] [text n, text (toString at)]) (zip names atomTypes))
    vView = List.map (uncurry tpRenderer) (Dict.toList (.values node))
  in
    table [] (tView :: vView)

atomViewer : AtomDef -> ClValue -> Html a
atomViewer ad = case ad of
    (ADTime _) -> \v -> case v of
        (ClTime (s, f)) -> text (String.concat [toString s, ":", toString f])
        _ -> text "Time did not contain time"
    (ADEnum opts) -> \v -> case v of
        (ClEnum e) -> text (case itemAtIndex e opts of
            Nothing -> "Option out of range for enum"
            Just o -> o)
        _ -> text "enum did not contain enum"
    (ADString s) -> \v -> case v of
        (ClString s) -> text s
        _ -> text (String.append "Expected string item got: " (toString v))
    (ADList sad) -> \v -> case v of
        (ClList items) -> span [] (List.map (atomViewer sad) items)
        _ -> text "List did not contain list"
    (ADSet sad) -> \v -> case v of
        (ClList items) -> span [] (List.map (atomViewer sad) items)
        _ -> text "Set did not contain set"
    _ -> text << toString  -- FIXME: this is pants

atomEditor : AtomDef -> ClValue -> Html AtomEdit
atomEditor ad = case ad of
    (ADEnum opts) -> \v -> case v of
        (ClEnum e) -> select
            [onInput (ClEnum << Result.withDefault -1 << String.toInt)]
            (List.indexedMap (\i o -> option [value (toString i), selected (i == e)] [text o]) opts)
        _ -> text "Enum did not contain enum"
    _ -> atomViewer ad
