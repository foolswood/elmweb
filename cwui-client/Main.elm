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
import ClMsgTypes exposing (..)
import Futility exposing (..)
import PathManipulation exposing (splitBasename)
import MonoTime

main = Html.program {
    init = init, update = update, subscriptions = subscriptions, view = view}

wsTarget : String
wsTarget = "ws://localhost:8004"

-- Model

-- type alias TypeNameKey = String

-- typeNameKey : TypeName -> TypeNameKey
-- typeNameKey {ns, seg} = ns ++ ":" ++ seg

type alias TypeMap = Dict TypeName Definition
type alias TypeAssignMap = Dict Path (TypeName, Liberty)
type alias NodeMap = Dict Path Node

type alias Model =
  { types : TypeMap
  , tyAssns : TypeAssignMap
  , nodes : NodeMap
  , errors : List String
  -- FIXME: This is pants, need a better story for UI state
  , partialEntry : String
  }

init : (Model, Cmd Msg)
init = (Model Dict.empty Dict.empty Dict.empty [] "somestring", Cmd.none)

-- Update
type InterfaceEvent
  = UpPartial String
  | IfSub Path
  | DataChange NodeEdit

sendBundle : ToRelayClientBundle -> Cmd Msg
sendBundle b = timeStamped (WebSocket.send wsTarget << serialiseBundle b)

subToCmd : List Path -> Cmd Msg
subToCmd ps = case ps of
    [] -> Cmd.none
    _ -> sendBundle (ToRelayClientBundle (List.map MsgSub ps) [] [])

interfaceUpdate : InterfaceEvent -> Model -> (Model, Cmd Msg)
interfaceUpdate ie model = case ie of
    UpPartial s -> ({model | partialEntry = s}, Cmd.none)
    IfSub s -> ({model | nodes = Dict.insert s unpopulatedNode (.nodes model)}, subToCmd [s])
    -- FIXME: DOES NOTHING!
    DataChange dc -> (model, Cmd.none)

handleDataUpdateMsg : DataUpdateMsg -> Node -> Node
handleDataUpdateMsg dum = case dum of
    MsgConstSet {msgTypes, msgArgs, msgAttributee} ->
        setConstData msgTypes (msgAttributee, msgArgs)
    (MsgSet
      { msgTpId, msgTime, msgTypes, msgArgs, msgInterpolation
      , msgAttributee}) ->
        setTimePoint msgTypes msgTpId msgTime msgAttributee msgArgs msgInterpolation
    (MsgRemove {msgTpId, msgAttributee}) ->
        removeTimePoint msgTpId msgAttributee

updateNode : (Node -> Node) -> Path -> NodeMap -> NodeMap
updateNode t p m =
  let
    -- FIXME: Doesn't note stuff from the blue anywhere?
    wt mn = Just (t (Maybe.withDefault unpopulatedNode mn))
  in
    Dict.update p wt m

handleDums : List DataUpdateMsg -> NodeMap -> NodeMap
handleDums dums nodeMap = List.foldl (\dum nm -> updateNode (handleDataUpdateMsg dum) (dumPath dum) nm) nodeMap dums

handleCms : List ContainerUpdateMsg -> NodeMap -> NodeMap
handleCms cms nodeMap =
  let
    handleCm cm = case cm of
        MsgPresentAfter {msgPath, msgTgt, msgRef, msgAttributee} ->
            updateNode (childPresentAfter msgAttributee msgTgt msgRef) msgPath
        MsgAbsent {msgPath, msgTgt, msgAttributee} ->
            updateNode (childAbsent msgAttributee msgTgt) msgPath
  in List.foldl handleCm nodeMap cms

handleDefOps : List DefMsg -> TypeMap -> TypeMap
handleDefOps defs types =
  let
    applyDefOp o = case o of
        MsgDefine tn def -> Dict.insert tn def
        MsgUndefine tn -> Dict.remove tn
  in List.foldl applyDefOp types defs

digestTypeMsgs : List TypeMsg -> TypeAssignMap
digestTypeMsgs tms =
  let
    applyTypeMsg (MsgAssignType p tn l) = (p, (tn, l))
  in Dict.fromList <| List.map applyTypeMsg tms

handleFromRelayBundle
  :  FromRelayClientBundle -> NodeMap -> TypeAssignMap -> TypeMap
  -> (NodeMap, TypeAssignMap, TypeMap, List String)
handleFromRelayBundle
    (FromRelayClientBundle typeUnsubs dataUnsubs errs defs typeAssns dms cms)
    nodes assigns types =
  let
    updatedTypes = handleDefOps defs <| dropKeys typeUnsubs types
    newAssigns = digestTypeMsgs typeAssns
    updatedAssigns = Dict.union newAssigns <| dropKeys dataUnsubs assigns
    -- FIXME: Should we insert empty nodes at the fresh assignments?
    updatedNodes = handleCms cms <| handleDums dms <| dropKeys dataUnsubs nodes
    -- FIXME: Crappy error handling
    globalErrs = List.map toString errs
  in (updatedNodes, updatedAssigns, updatedTypes, globalErrs)

type Msg
  = GlobalError String
  | InterfaceEvent InterfaceEvent
  | NetworkEvent FromRelayClientBundle
  | TimeStamped (Time -> Cmd Msg) Time.Time

timeStamped : (Time -> Cmd Msg) -> Cmd Msg
timeStamped c = Task.perform (TimeStamped c) MonoTime.now

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    (NetworkEvent b) ->
      let
        (newNodes, newAssns, newTypes, globalErrs) = handleFromRelayBundle b (.nodes model) (.tyAssns model) (.types model)
      in
        ({model | nodes = newNodes, tyAssns = newAssns, types = newTypes, errors = globalErrs ++ .errors model}, Cmd.none)
    (InterfaceEvent ie) -> interfaceUpdate ie model
    (GlobalError s) -> ({model | errors = s :: .errors model}, Cmd.none)
    (TimeStamped c t) -> (model, c (fromFloat t))

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model = WebSocket.listen wsTarget eventFromNetwork

eventFromNetwork : String -> Msg
eventFromNetwork s = case parseBundle s of
    (Ok b) -> NetworkEvent b
    (Err e) -> GlobalError e

-- View

view : Model -> Html Msg
view {errors, types, tyAssns, nodes, partialEntry} = Html.map InterfaceEvent <|
    div []
      [ viewErrors errors
      , subControl partialEntry
      , Html.map DataChange <| viewPaths types tyAssns nodes
      ]

viewErrors : List String -> Html a
viewErrors errs = ul [] (List.map (\s -> li [] [text s]) errs)

subControl : String -> Html InterfaceEvent
subControl path = div []
  [ input [type_ "text", placeholder "Path", onInput (UpPartial)] []
  , button [onClick (IfSub path)] [text "sub"]
  ]

viewPaths : TypeMap -> TypeAssignMap -> NodeMap -> Html NodeEdit
viewPaths types tyAssns nodes =
  let
    renderPath p n = case Dict.get p tyAssns of
        Nothing -> text <| "Path missing from assignment map: " ++ p
        Just (tn, lib) -> case Dict.get tn types of
            Nothing -> text <| "Type missing from map: " ++ toString tn
            Just ty -> div []
              [ h2 [] [text p] 
              , Html.map (ConstDataEdit p) <| renderNode lib ty n
              ]
  in div [] <| List.map (uncurry renderPath) (Dict.toList nodes)

type NodeEdit
  = ConstDataEdit Path TupleEdit
--  | TimePointEdit Path TpId Time TupleEdit

withErrorBox : List String -> Html a -> Html a
withErrorBox errs content = case errs of
    [] -> content
    _ -> div []
      [ text <| toString errs
      , content
      ]

renderNode : Liberty -> Definition -> Node -> Html TupleEdit
renderNode lib def node = withErrorBox (.errors node) <| case def of
    StructDef d -> case .body node of
        ContainerNode n -> renderStructNode d n
        _ -> text "Expecting a container for struct"
    ArrayDef d -> case .body node of
        ContainerNode n -> renderArrayNode lib d n
        _ -> text "Expecting a container for array"
    TupleDef d -> case .body node of
        ConstDataNode n -> renderConstDataNode lib d n
        TimeSeriesNode n -> renderTimeSeriesDataNode lib d n
        _ -> text "Expecting a data node for tuple"

type alias AtomEdit = WireValue
type alias TupleEdit = List AtomEdit

renderArrayNode : Liberty -> ArrayDefinition -> ContainerNodeT -> Html a
renderArrayNode lib ad cn = text "Array node rendering not implemented yet"

renderStructNode : StructDefinition -> ContainerNodeT -> Html a
renderStructNode sd cn = text "Struct node rendering not implemented yet"

renderTimeSeriesDataNode : Liberty -> TupleDefinition -> TimeSeriesNodeT -> Html a
renderTimeSeriesDataNode lib td tsn = text "Time series rendering not implemented yet"

renderConstDataNode : Liberty -> TupleDefinition -> ConstDataNodeT -> Html TupleEdit
renderConstDataNode l td n =
  let
    defs = List.map Tuple.second <| .types td
    wvs = Tuple.second <| .values n
    updateValue idx nv = List.indexedMap (\i v -> if i == idx then nv else v) wvs
    editorFor idx def wv = Html.map (updateValue idx) <| atomEditor def wv
    indices = List.range 0 <| List.length wvs
    atomRenderers = case l of
        Cannot -> List.map2 atomViewer defs wvs
        _ -> List.map3 editorFor indices defs wvs
  in span [] atomRenderers

atomViewer : AtomDef -> WireValue -> Html a
atomViewer ad = case ad of
    (ADTime _) -> \v -> case v of
        (WvTime (s, f)) -> text (String.concat [toString s, ":", toString f])
        _ -> text "Time did not contain time"
    (ADEnum opts) -> \v -> case v of
        (WvWord8 e) -> text (case itemAtIndex e opts of
            Nothing -> "Option out of range for enum"
            Just o -> o)
        _ -> text "enum did not contain enum"
    (ADString s) -> \v -> case v of
        (WvString s) -> text s
        _ -> text (String.append "Expected string item got: " (toString v))
    (ADList sad) -> \v -> case v of
        (WvList items) -> span [] (List.map (atomViewer sad) items)
        _ -> text "List did not contain list"
    (ADSet sad) -> \v -> case v of
        (WvList items) -> span [] (List.map (atomViewer sad) items)
        _ -> text "Set did not contain set"
    _ -> text << toString  -- FIXME: this is pants

atomEditor : AtomDef -> WireValue -> Html AtomEdit
atomEditor ad = case ad of
    (ADEnum opts) -> \v -> case v of
        (WvWord8 e) -> Html.map WvWord8 <| enumEditor opts e
        _ -> text "Enum value not word8"
    (ADTime opts) -> \v -> case v of
        (WvTime (s, f)) -> input
            [ type_ "range"
            , Html.Attributes.min "0"
            , Html.Attributes.max "30"
            , value (toString s)
            , onInput (\s -> WvTime ((Result.withDefault -1 (String.toInt s)), 0))]
            []
        _ -> text "Time did not contain time"
    _ -> atomViewer ad

enumEditor : List String -> Int -> Html Int
enumEditor opts e = select
    [onInput (Result.withDefault -1 << String.toInt)]
    (List.indexedMap (\i o -> option [value (toString i), selected (i == e)] [text o]) opts)
