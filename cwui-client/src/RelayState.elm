module RelayState exposing (TypeMap, TypeAssignMap, NodeMap, handleFromRelayBundle)

import Dict exposing (Dict)

import Futility exposing (dropKeys)
import ClTypes exposing (Definition, TypeName, Liberty, Path)
import ClMsgTypes exposing (FromRelayClientBundle(..), TypeMsg(..), DefMsg(..), ContainerUpdateMsg(..), DataUpdateMsg(..), dumPath)
import ClNodes exposing (Node, childAbsent, childPresentAfter, unpopulatedNode, removeTimePoint, setTimePoint, setConstData)

type alias TypeMap = Dict TypeName Definition
type alias TypeAssignMap = Dict Path (TypeName, Liberty)
type alias NodeMap = Dict Path Node

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
