module Digests exposing
  ( Digest, digest, applyDigest, TaOp(..), Cops, DataChange(..), ConstChangeT
  , constChangeCast, TimeChangeT, seriesChangeCast, TimeSeriesDataOp(..)
  , DataDigest)

import Dict exposing (Dict)

import ClTypes exposing
  ( TypeName, Path, Seg, Namespace, TpId, Interpolation, Time, Attributee
  , WireValue, WireType, Definition, PostDefinition, Editable)
import ClMsgTypes exposing
  ( FromRelayClientBundle(..), FromRelayClientUpdateBundle(..), TypeMsg(..)
  , DefMsg(..) , ToProviderContainerUpdateMsg(..), ToClientContainerUpdateMsg(..)
  , DataUpdateMsg(..), dumPath, DataErrorIndex(..), MsgError(..))
import ClNodes exposing (childUpdate, removeTimePoint, setTimePoint, setConstData)
import SequenceOps exposing (SeqOp(..))
import RemoteState exposing (RemoteState, NodeMap, TypeAssignMap, TypeMap)

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

type alias ConstChangeT = (Maybe Attributee, List WireType, List WireValue)
type alias TimeChangeT = (Maybe Attributee, TimeSeriesDataOp)

type DataChange
  = ConstChange ConstChangeT
  | TimeChange (Dict TpId TimeChangeT)

constChangeCast : DataChange -> Result String ConstChangeT
constChangeCast dc = case dc of
    ConstChange cc -> Ok cc
    _ -> Err "Not ConstChange"

seriesChangeCast : DataChange -> Result String (Dict TpId TimeChangeT)
seriesChangeCast dc = case dc of
    TimeChange tc -> Ok tc
    _ -> Err "Not TimeChange"

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
            Just <| ConstChange (msgAttributee, msgTypes, msgArgs)
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
        ConstChange (ma, wts, wvs) -> case setConstData wts (ma, wvs) mn of
            Ok newN -> (errs, Dict.insert p newN nm)
            Err msg -> ((p, (Nothing, msg)) :: errs, nm)
        TimeChange d ->
          let
            (tpErrs, newMn) = Dict.foldl tcApply ([], mn) d
            newErrs = List.map (\(tpId, msg) -> (p, (Just tpId, msg))) tpErrs ++ errs
          in (newErrs, Dict.update p (\_ -> newMn) nm)
  in Dict.foldl dcApply ([], nodeMap) dd

type alias Cops = Dict Seg (Maybe Attributee, SeqOp Seg)
type alias CmDigest = Dict Path Cops

cmUnion : CmDigest -> CmDigest -> CmDigest
cmUnion cmA cmB =
  let
    combine k a b = Dict.insert k <| Dict.union a b
  in Dict.merge Dict.insert combine Dict.insert cmA cmB Dict.empty

digestCCms : List ToClientContainerUpdateMsg -> CmDigest
digestCCms =
  let
    wedgeIn k v md = Just <| case md of
        Nothing -> Dict.singleton k v
        Just d -> Dict.insert k v d
    digestCm cm = case cm of
        MsgPresentAfter {msgPath, msgTgt, msgRef, msgAttributee} ->
            Dict.update msgPath <| wedgeIn msgTgt (msgAttributee, SoPresentAfter msgRef)
        MsgAbsent {msgPath, msgTgt, msgAttributee} ->
            Dict.update msgPath <| wedgeIn msgTgt (msgAttributee, SoAbsent)
  in List.foldl digestCm Dict.empty

applyCms : CmDigest -> NodeMap -> (List (Path, String), NodeMap)
applyCms cms nm =
  let
    applyOps p ops = failyUpdate (childUpdate ops) p
  in Dict.foldl applyOps ([], nm) cms

type DefOp a
  = OpDefine a
  | OpUndefine

type alias DefOps a = Dict TypeName (DefOp a)

digestDefOps : Namespace -> List (DefMsg a) -> DefOps a
digestDefOps ns defs =
  let
    applyDefOp o = case o of
        MsgDefine s def -> Dict.insert (ns, s) <| OpDefine def
        MsgUndefine s -> Dict.insert (ns, s) OpUndefine
  in List.foldl applyDefOp Dict.empty defs

applyDefOps : DefOps a -> TypeMap a -> TypeMap a
applyDefOps dops tm =
  let
    applyOp tn op = case op of
        OpDefine def -> Dict.insert tn def
        OpUndefine -> Dict.remove tn
  in Dict.foldl applyOp tm dops

type TaOp
  = OpAssign (TypeName, Editable)
  | OpDemote

type alias TaOps = Dict Path TaOp

digestTypeMsgs : List TypeMsg -> TaOps
digestTypeMsgs tms =
  let
    tas = List.map (\(MsgAssignType p tn l) -> (p, OpAssign (tn, l))) tms
  in Dict.fromList tas

dropDemotes : TaOps -> Dict Path a -> Dict Path a
dropDemotes tao d =
  let
    doDrop p op = case op of
        OpDemote -> Dict.remove p
        _ -> identity
  in Dict.foldl doDrop d tao

applyTypeMsgs : TaOps -> TypeAssignMap -> TypeAssignMap
applyTypeMsgs tao tam =
  let
    applyOp p op = case op of
        OpAssign a -> Dict.insert p a
        OpDemote -> Dict.remove p
  in Dict.foldl applyOp tam tao

type alias Digest =
  { postDefs : DefOps PostDefinition
  , defs : DefOps Definition
  , taOps : TaOps
  , cops : CmDigest
  , dops : DataDigest
  , errs : List (DataErrorIndex, String)
  }

-- You can't write mapKeys because comparable
nsPathDict : Namespace -> Dict Path a -> Dict Path a
nsPathDict ns = Dict.fromList << List.map (\(k, v) -> ("/" ++ ns ++ "/" ++ k, v)) << Dict.toList

digestFrcub : FromRelayClientUpdateBundle -> Digest
digestFrcub (FromRelayClientUpdateBundle ns errs pDefs defs tas dums cms) =
  { defs = digestDefOps ns defs
  , postDefs = digestDefOps ns pDefs
  , taOps = nsPathDict ns <| digestTypeMsgs tas
  , cops = nsPathDict ns <| digestCCms cms
  , dops = nsPathDict ns <| digestDums dums
  -- These are not namespaced:
  , errs = List.map (\(MsgError idx s) -> (idx, s)) errs
  }

-- Obviously, this wants to die
doNothingDigest : a -> Digest
doNothingDigest _ =
  { postDefs = Dict.empty
  , defs = Dict.empty
  , taOps = Dict.empty
  , cops = Dict.empty
  , dops = Dict.empty
  , errs = []
  }

digest : FromRelayClientBundle -> Digest
digest b = case b of
    Frcub ub -> digestFrcub ub
    Frseb eb -> doNothingDigest eb
    Frrub rb -> doNothingDigest rb

applyDigest : Digest -> RemoteState -> (RemoteState, List (DataErrorIndex, String))
applyDigest d rs =
  let
    newTm = applyDefOps (.defs d) <| .types rs
    newPtm = applyDefOps (.postDefs d) <| .postTypes rs
    newTam = applyTypeMsgs (.taOps d) <| .tyAssns rs
    reducedNodes = dropDemotes (.taOps d) <| .nodes rs
    (dataErrs, dataAppliedNm) = ddApply (.dops d) reducedNodes
    (contErrs, newNm) = applyCms (.cops d) dataAppliedNm
    indexDumErr (p, (mTpId, s)) = case mTpId of
        Nothing -> (DPathError p, s)
        Just tpId -> (DTimePointError p tpId, s)
    indexedErrs
       = .errs d
      ++ List.map indexDumErr dataErrs
      ++ List.map (\(p, s) -> (DPathError p, s)) contErrs
  in ({types = newTm, postTypes = newPtm, tyAssns = newTam, nodes = newNm}, indexedErrs)
