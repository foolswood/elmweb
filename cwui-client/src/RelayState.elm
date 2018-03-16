module RelayState exposing (TypeMap, TypeAssignMap, NodeMap, handleFromRelayBundle)

import Dict exposing (Dict)

import Futility exposing (dropKeys)
import ClTypes exposing (Definition, TypeName, Liberty, Path, Seg, TpId, Interpolation, Time, Attributee, WireValue, WireType)
import ClMsgTypes exposing (FromRelayClientBundle(..), TypeMsg(..), DefMsg(..), ContainerUpdateMsg(..), DataUpdateMsg(..), dumPath, ErrorIndex(..), MsgError(..))
import ClNodes exposing (Node, childUpdate, removeTimePoint, setTimePoint, setConstData, TimePoint, TimeSeriesNodeT)
import SequenceOps exposing (SeqOp(..))

type alias TypeMap = Dict TypeName Definition
type alias TypeAssignMap = Dict Path (TypeName, Liberty)
type alias NodeMap = Dict Path Node

failyUpdate
   : (Maybe a -> Result e a) -> comparable
   -> (List (comparable, e), Dict comparable a)
   -> (List (comparable, e), Dict comparable a)
failyUpdate f k (es, d) = case f <| Dict.get k d of
    Ok v -> (es, Dict.insert k v d)
    Err e -> ((k, e) :: es, d)

type TimeSeriesDataOp
  = OpSet Time (List WireType) (List WireValue) Interpolation
  | OpRemove

type alias TimeChangeT = Dict TpId (Maybe Attributee, TimeSeriesDataOp)

type DataChange
  = ConstChange (Maybe Attributee) (List WireType) (List WireValue)
  | TimeChange TimeChangeT

type alias DataDigest = Dict Path DataChange

-- Left biased union
ddUnion : DataDigest -> DataDigest -> DataDigest
ddUnion ddA ddB =
  let
    combine k a b = Dict.insert k <| case (a, b) of
        (TimeChange ad, TimeChange bd) ->
            TimeChange <| Dict.union ad bd
        _ -> a
  in Dict.merge Dict.insert combine Dict.insert ddA ddB Dict.empty

digestDum : DataUpdateMsg -> DataDigest -> DataDigest
digestDum dum =
  let
    insertTs tpid ma tso mv = Just <| TimeChange <| case mv of
        Just (TimeChange d) -> Dict.insert tpid (ma, tso) d
        _ -> Dict.singleton tpid (ma, tso)
    up mv = case dum of
        MsgConstSet {msgTypes, msgArgs, msgAttributee} ->
            Just <| ConstChange msgAttributee msgTypes msgArgs
        MsgSet {msgTpId, msgTime, msgTypes, msgArgs, msgInterpolation, msgAttributee} ->
          let
            atp = OpSet msgTime msgTypes msgArgs msgInterpolation
          in insertTs msgTpId msgAttributee atp mv
        MsgRemove {msgTpId, msgAttributee} ->
            insertTs msgTpId msgAttributee OpRemove mv
  in Dict.update (dumPath dum) up

digestDums : List DataUpdateMsg -> DataDigest
digestDums = List.foldl digestDum Dict.empty

ddApply : DataDigest -> NodeMap -> (List (Path, (Maybe TpId, String)), NodeMap)
ddApply dd nodeMap =
  let
    tcApply tpId (ma, tso) (errs, mn) =
      let
        r = case tso of
            OpSet t wts wvs i -> setTimePoint wts tpId t ma wvs i mn
            OpRemove -> removeTimePoint tpId ma mn
      in case r of
            Ok n -> (errs, Just n)
            Err msg -> ((tpId, msg) :: errs, mn)
    -- FIXME: Cleaner with failyUpdate?
    dcApply p dc (errs, nm) =
      let
        mn = Dict.get p nm
      in case dc of
        ConstChange ma wts wvs -> case setConstData wts (ma, wvs) mn of
            Ok newN -> (errs, Dict.insert p newN nm)
            Err msg -> ((p, (Nothing, msg)) :: errs, nm)
        TimeChange d ->
          let
            (tpErrs, newMn) = Dict.foldl tcApply ([], mn) d
            newErrs = List.map (\(tpId, msg) -> (p, (Just tpId, msg))) tpErrs ++ errs
          in (newErrs, Dict.update p (\_ -> newMn) nm)
  in Dict.foldl dcApply ([], nodeMap) dd

handleDums : List DataUpdateMsg -> NodeMap -> (List (Path, (Maybe TpId, String)), NodeMap)
handleDums dums nodeMap = ddApply (digestDums dums) nodeMap

-- FIXME: Drops attributee
digestCms : List ContainerUpdateMsg -> Dict Path (Dict Seg (SeqOp Seg))
digestCms =
  let
    wedgeIn k v md = Just <| case md of
        Nothing -> Dict.singleton k v
        Just d -> Dict.insert k v d
    digestCm cm = case cm of
        MsgPresentAfter {msgPath, msgTgt, msgRef, msgAttributee} ->
            Dict.update msgPath (wedgeIn msgTgt <| SoPresentAfter msgRef)
        MsgAbsent {msgPath, msgTgt, msgAttributee} ->
            Dict.update msgPath (wedgeIn msgTgt <| SoAbsent)
  in List.foldl digestCm Dict.empty

handleCms : List ContainerUpdateMsg -> NodeMap -> (List (Path, String), NodeMap)
handleCms cms nodeMap =
  let
    dcm = digestCms cms
    applyOps p ops = failyUpdate (childUpdate ops) p
  in Dict.foldl applyOps ([], nodeMap) dcm

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
  -> (NodeMap, TypeAssignMap, TypeMap, List (ErrorIndex, String))
handleFromRelayBundle
    (FromRelayClientBundle typeUnsubs dataUnsubs errMsgs defs typeAssns dms cms)
    nodes assigns types =
  let
    updatedTypes = handleDefOps defs <| dropKeys typeUnsubs types
    newAssigns = digestTypeMsgs typeAssns
    updatedAssigns = Dict.union newAssigns <| dropKeys dataUnsubs assigns
    -- FIXME: Clear nodes whose types have changed
    (dataErrs, updatedDataNodes) = handleDums dms <| dropKeys dataUnsubs nodes
    (contErrs, updatedNodes) = handleCms cms updatedDataNodes
    indexDumErr (p, (mTpId, s)) = case mTpId of
        Nothing -> (PathError p, s)
        Just tpId -> (TimePointError p tpId, s)
    indexedErrs
       = List.map (\(MsgError idx s) -> (idx, s)) errMsgs
      ++ List.map indexDumErr dataErrs
      ++ List.map (\(p, s) -> (PathError p, s)) contErrs
  in (updatedNodes, updatedAssigns, updatedTypes, indexedErrs)
