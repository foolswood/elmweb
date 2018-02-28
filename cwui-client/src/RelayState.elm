module RelayState exposing (TypeMap, TypeAssignMap, NodeMap, handleFromRelayBundle)

import Dict exposing (Dict)

import Futility exposing (dropKeys)
import ClTypes exposing (Definition, TypeName, Liberty, Path, TpId)
import ClMsgTypes exposing (FromRelayClientBundle(..), TypeMsg(..), DefMsg(..), ContainerUpdateMsg(..), DataUpdateMsg(..), dumPath)
import ClNodes exposing (Node, childAbsent, childPresentAfter, removeTimePoint, setTimePoint, setConstData)

type alias TypeMap = Dict TypeName Definition
type alias TypeAssignMap = Dict Path (TypeName, Liberty)
type alias NodeMap = Dict Path Node

handleDataUpdateMsg : DataUpdateMsg -> Maybe Node -> Result (Maybe TpId, String) Node
handleDataUpdateMsg dum = case dum of
    MsgConstSet {msgTypes, msgArgs, msgAttributee} -> Result.mapError (\s -> (Nothing, s)) <<
        setConstData msgTypes (msgAttributee, msgArgs)
    (MsgSet
      { msgTpId, msgTime, msgTypes, msgArgs, msgInterpolation
      , msgAttributee}) -> Result.mapError (\s -> (Just msgTpId, s)) <<
        setTimePoint msgTypes msgTpId msgTime msgAttributee msgArgs msgInterpolation
    (MsgRemove {msgTpId, msgAttributee}) -> Result.mapError (\s -> (Just msgTpId, s)) <<
        removeTimePoint msgTpId msgAttributee

failyUpdate
   : (Maybe a -> Result e a) -> comparable
   -> (List (comparable, e), Dict comparable a)
   -> (List (comparable, e), Dict comparable a)
failyUpdate f k (es, d) = case f <| Dict.get k d of
    Ok v -> (es, Dict.insert k v d)
    Err e -> ((k, e) :: es, d)

handleDums : List DataUpdateMsg -> NodeMap -> (List (Path, (Maybe TpId, String)), NodeMap)
handleDums dums nodeMap = List.foldl
    (\dum -> failyUpdate (handleDataUpdateMsg dum) (dumPath dum))
    ([], nodeMap)
    dums

handleCms : List ContainerUpdateMsg -> NodeMap -> (List (Path, String), NodeMap)
handleCms cms nodeMap =
  let
    handleCm cm = case cm of
        MsgPresentAfter {msgPath, msgTgt, msgRef, msgAttributee} ->
            failyUpdate (childPresentAfter msgAttributee msgTgt msgRef) msgPath
        MsgAbsent {msgPath, msgTgt, msgAttributee} ->
            failyUpdate (childAbsent msgAttributee msgTgt) msgPath
  in List.foldl handleCm ([], nodeMap) cms

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
    -- FIXME: Clear nodes whose types have changed
    (dataErrs, updatedDataNodes) = handleDums dms <| dropKeys dataUnsubs nodes
    (contErrs, updatedNodes) = handleCms cms updatedDataNodes
    -- FIXME: Crappy error handling
    globalErrs = [toString (errs, dataErrs, contErrs)]
  in (updatedNodes, updatedAssigns, updatedTypes, globalErrs)
