module Digests exposing
  ( Digest, digest, applyDigest, TaOp(..), Cops, DataChange(..), ConstChangeT
  , constChangeCast, TimeChangeT, seriesChangeCast, TimeSeriesDataOp(..)
  , DataDigest
  , digestFrcub, digestFrseb, digestFrrub)

import Dict exposing (Dict)
import Set exposing (Set)

import Cmp.Set as CSet
import Cmp.Dict as CDict
import Tagged.Tagged as T exposing (Tagged(..))
import Tagged.Dict as TD exposing (TaggedDict)
import Tagged.Set as TS exposing (TaggedSet)
import ClTypes exposing
  ( Path, Seg, Namespace, TpId, Interpolation, Time, Attributee
  , WireValue, WireType, Definition, PostDefinition, Editable, TypeName
  , typeNameGetNs, typeNameGetSeg)
import ClMsgTypes exposing
  ( FromRelayClientBundle(..), FromRelayClientUpdateBundle(..)
  , FromRelaySubErrorBundle(..), FromRelayRootBundle(..), TypeMsg(..)
  , DefMsg(..) , ToProviderContainerUpdateMsg(..), ToClientContainerUpdateMsg(..)
  , DataUpdateMsg(..), dumPath, DataErrorIndex(..), MsgError(..), SubErrorIndex(..))
import ClNodes exposing (childUpdate, removeTimePoint, setTimePoint, setConstData)
import SequenceOps exposing (SeqOp(..))
import RemoteState exposing (RemoteState, NodeMap, TypeAssignMap, TypeMap, Valuespace, vsEmpty, ByNs)

failyUpdate
   : (Maybe a -> Result e a) -> (e -> e1) -> comparable
   -> (List (comparable, e1), Dict comparable a)
   -> (List (comparable, e1), Dict comparable a)
failyUpdate f errConv k (es, d) = case f <| Dict.get k d of
    Ok v -> (es, Dict.insert k v d)
    Err e -> ((k, errConv e) :: es, d)

type TimeSeriesDataOp
  = OpSet Time (List WireType) (List WireValue) Interpolation
  | OpRemove

type alias ConstChangeT = (Maybe Attributee, List WireType, List WireValue)
type alias TimeChangeT = (Maybe Attributee, TimeSeriesDataOp)

type DataChange
  = ConstChange ConstChangeT
  | TimeChange (Dict TpId TimeChangeT)
  | DeletedChange

constChangeCast : DataChange -> Result String ConstChangeT
constChangeCast dc = case dc of
    ConstChange cc -> Ok cc
    _ -> Err "Not ConstChange"

seriesChangeCast : DataChange -> Result String (Dict TpId TimeChangeT)
seriesChangeCast dc = case dc of
    TimeChange tc -> Ok tc
    _ -> Err "Not TimeChange"

type alias DataDigest = Dict Path DataChange

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
    dcApply p dc (errs, nm) = case dc of
        ConstChange (ma, wts, wvs) -> failyUpdate
            (setConstData wts (ma, wvs)) (\e -> (Nothing, e)) p (errs, nm)
        TimeChange d ->
          let
            (tpErrs, newMn) = Dict.foldl tcApply ([], Dict.get p nm) d
            newErrs = List.map (\(tpId, msg) -> (p, (Just tpId, msg))) tpErrs ++ errs
          in (newErrs, Dict.update p (\_ -> newMn) nm)
        DeletedChange -> (errs, Dict.remove p nm)  -- FIXME: Too idempotent?
  in Dict.foldl dcApply ([], nodeMap) dd

type alias Cops = Dict Seg (Maybe Attributee, SeqOp Seg)
type alias CmDigest = Dict Path Cops

digestCm : ToClientContainerUpdateMsg -> (Seg, (Maybe Attributee, SeqOp Seg))
digestCm cm = case cm of
    MsgPresentAfter {msgTgt, msgRef, msgAttributee} ->
        (msgTgt, (msgAttributee, SoPresentAfter msgRef))
    MsgAbsent {msgTgt, msgAttributee} ->
        (msgTgt, (msgAttributee, SoAbsent))

digestCCms : List (Path, ToClientContainerUpdateMsg) -> CmDigest
digestCCms =
  let
    wedgeIn k v md = Just <| case md of
        Nothing -> Dict.singleton k v
        Just d -> Dict.insert k v d
    go (p, cm) = let (seg, (ma, so)) = digestCm cm in
        Dict.update p (wedgeIn seg (ma, so))
  in List.foldl go Dict.empty

applyCms : CmDigest -> NodeMap -> (List (Path, String), NodeMap)
applyCms cms nm =
  let
    applyOps p ops = failyUpdate (childUpdate ops) identity p
  in Dict.foldl applyOps ([], nm) cms

type DefOp a
  = OpDefine a
  | OpUndefine

type alias DefOps a = TaggedDict a Seg (DefOp a)

digestDefOps : List (DefMsg a) -> DefOps a
digestDefOps defs =
  let
    applyDefOp o = case o of
        MsgDefine s def -> CDict.insert s <| OpDefine def
        MsgUndefine s -> CDict.insert s OpUndefine
  in List.foldl applyDefOp TD.empty defs

applyDefOps : DefOps a -> TypeMap a -> TypeMap a
applyDefOps dops tm =
  let
    applyOp tn op = case op of
        OpDefine def -> CDict.insert tn def
        OpUndefine -> CDict.remove tn
  in CDict.foldl applyOp tm dops

type TaOp
  = OpAssign (Tagged Definition Seg, Editable)
  | OpDemote

type alias TaOps = Dict Path TaOp

digestTypeMsgs : List TypeMsg -> TaOps
digestTypeMsgs tms =
  let
    tas = List.map (\(MsgAssignType p ts l) -> (p, OpAssign (ts, l))) tms
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

type alias NsDigest =
  { postDefs : DefOps PostDefinition
  , defs : DefOps Definition
  , taOps : TaOps
  , cops : CmDigest
  , dops : DataDigest
  , errs : List (DataErrorIndex, String)
  }

type alias Digest =
  { nsds : ByNs NsDigest
  , rootCops : Cops
  , subErrs : List (SubErrorIndex, String)
  }

unErrMsg : MsgError i -> (i, String)
unErrMsg (MsgError i s) = (i, s)

digestFrcub : FromRelayClientUpdateBundle -> Digest
digestFrcub (FromRelayClientUpdateBundle ns errs pDefs defs tas dums cms) =
  { rootCops = Dict.empty
  , nsds = TD.singleton ns <|
      { defs = digestDefOps defs
      , postDefs = digestDefOps pDefs
      , taOps = digestTypeMsgs tas
      , cops = digestCCms cms
      , dops = digestDums dums
      , errs = List.map unErrMsg errs
      }
  , subErrs = []
  }

digestFrseb : FromRelaySubErrorBundle -> Digest
digestFrseb (FromRelaySubErrorBundle errs pUnsubs tUnsubs dUnsubs) =
  let
    insertUndef ts = CDict.insert ts OpUndefine
    insertUnsub p = Dict.insert p DeletedChange
    nsOrient = List.foldl
        (\tn -> CDict.update
            (typeNameGetNs tn)
            (Just << (\a -> typeNameGetSeg tn :: a) << Maybe.withDefault []))
        TD.empty
    nsoPuns = nsOrient pUnsubs
    nsoTuns = nsOrient tUnsubs
    nsoDuns = List.foldl
        (\(Tagged (ns, p)) -> CDict.update (Tagged ns) (Just << (\a -> p :: a) << Maybe.withDefault []))
        TD.empty dUnsubs
    mentionedNss = TS.fromList <| CDict.keys nsoPuns ++ CDict.keys nsoTuns ++ CDict.keys nsoDuns
    forNs ns conv nsoUnsubs = case CDict.get ns nsoUnsubs of
        Nothing -> TD.empty
        Just unsubs -> List.foldl conv TD.empty unsubs
    genNsd ns = CDict.insert ns
      { postDefs = forNs ns insertUndef nsoPuns
      , defs = forNs ns insertUndef nsoTuns
      , dops = case CDict.get ns nsoDuns of
            Nothing -> Dict.empty
            Just unsubs -> List.foldl insertUnsub Dict.empty unsubs
      , taOps = Dict.empty
      , cops = Dict.empty
      , errs = []
      }
    nsds = CSet.foldl genNsd TD.empty mentionedNss
  in {rootCops = Dict.empty, nsds = nsds, subErrs = List.map unErrMsg errs}

digestFrrub : FromRelayRootBundle -> Digest
digestFrrub (FromRelayRootBundle contUps) =
  { rootCops = Dict.fromList <| List.map digestCm contUps
  , nsds = TD.empty
  , subErrs = []
  }

digest : FromRelayClientBundle -> Digest
digest b = case b of
    Frcub ub -> digestFrcub ub
    Frseb eb -> digestFrseb eb
    Frrub rb -> digestFrrub rb

applyNsDigest : NsDigest -> Valuespace -> (Valuespace, List (DataErrorIndex, String))
applyNsDigest d rs =
  let
    newTm = applyDefOps (.defs d) <| .types rs
    newPtm = Debug.log "PdApplied" <| applyDefOps (Debug.log "newdefs" <| .postDefs d) <| .postTypes rs
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

applyRootChanges : a -> Digest -> ByNs a -> ByNs a
applyRootChanges empty d bns =
  let
    applyRc s (_, so) = let ns = Tagged s in case so of
        SoAbsent -> CDict.remove ns
        SoPresentAfter _ -> CDict.update ns (Just << Maybe.withDefault empty)
  in Dict.foldl applyRc bns <| .rootCops d

applyDigest
   : Digest -> RemoteState
  -> (RemoteState, ByNs (List (DataErrorIndex, String)))
applyDigest d rs =
  let
    nsCorrectedVss = applyRootChanges vsEmpty d rs
    applyNsd ns nsd (ars, aerrs) = case CDict.get ns ars of
        Nothing ->
            (ars, CDict.insert ns [(DGlobalError, "Namespace missing from root")] aerrs)
        Just vs -> let (newVs, es) = applyNsDigest nsd vs in
            (CDict.insert ns newVs ars, CDict.insert ns es aerrs)
    (appliedVss, errs) = CDict.foldl applyNsd (nsCorrectedVss, TD.empty) <| .nsds d
  in (appliedVss, errs)
