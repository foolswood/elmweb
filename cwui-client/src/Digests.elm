module Digests exposing
  ( Digest, applyDigest, TaOp, Cops, DataChange(..), ConstChangeT
  , constChangeCast, TimeChangeT, seriesChangeCast, TimeSeriesDataOp(..)
  , DataDigest, DefOp(..), NsDigest, TrcUpdateDigest, CreateOp, TrcSubDigest
  , SubOp(..), ToRelayDigest(..), ntsCmp)

import Dict exposing (Dict)
import Set exposing (Set)

import Cmp.Cmp exposing (Cmp)
import Cmp.Set as CSet
import Cmp.Dict as CDict exposing (CmpDict)
import Tagged.Tagged as T exposing (Tagged(..))
import Tagged.Dict as TD exposing (TaggedDict)
import ClTypes exposing
  ( Path, Seg, Namespace, TpId, Interpolation, Time, Attributee, WireValue
  , WireType, Definition, PostDefinition, Editable, TypeName, SubPath
  , Placeholder, DataErrorIndex(..), SubErrorIndex(..))
import ClNodes exposing (childUpdate, removeTimePoint, setTimePoint, setConstData)
import SequenceOps exposing (SeqOp(..))
import RemoteState exposing (RemoteState, NodeMap, TypeAssignMap, TypeMap, Valuespace, vsEmpty, ByNs)
import Futility exposing (Either)

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

type alias Cops a = Dict Seg (Maybe Attributee, SeqOp a)
type alias CmDigest a = Dict Path (Cops a)

applyCms : CmDigest Seg -> NodeMap -> (List (Path, String), NodeMap)
applyCms cms nm =
  let
    applyOps p ops = failyUpdate (childUpdate ops) identity p
  in Dict.foldl applyOps ([], nm) cms

type DefOp a
  = OpDefine a
  | OpUndefine

type alias DefOps a = TaggedDict a Seg (DefOp a)

applyDefOps : DefOps a -> TypeMap a -> TypeMap a
applyDefOps dops tm =
  let
    applyOp tn op = case op of
        OpDefine def -> CDict.insert tn def
        OpUndefine -> CDict.remove tn
  in CDict.foldl applyOp tm dops

type alias TaOp = (Tagged Definition Seg, Editable)

type alias TaOps = Dict Path TaOp

applyTypeMsgs : TaOps -> TypeAssignMap -> TypeAssignMap
applyTypeMsgs tao tam =
  let
    applyOp p a = Dict.insert p a
  in Dict.foldl applyOp tam tao

type alias NsDigest =
  { postDefs : DefOps PostDefinition
  , defs : DefOps Definition
  , taOps : TaOps
  , cops : CmDigest Seg
  , dops : DataDigest
  , errs : List (DataErrorIndex, List String)
  }

type alias Digest =
  { nsds : ByNs NsDigest
  , rootCops : Dict Seg (SeqOp Seg)
  , subErrs : List (SubErrorIndex, List String)
  }

applyNsDigest : NsDigest -> Valuespace -> (Valuespace, List (DataErrorIndex, List String))
applyNsDigest d rs =
  let
    newTm = applyDefOps (.defs d) <| .types rs
    newPtm = Debug.log "PdApplied" <| applyDefOps (Debug.log "newdefs" <| .postDefs d) <| .postTypes rs
    newTam = applyTypeMsgs (.taOps d) <| .tyAssns rs
    (dataErrs, dataAppliedNm) = ddApply (.dops d) <| .nodes rs
    (contErrs, newNm) = applyCms (.cops d) dataAppliedNm
    indexDumErr (p, (mTpId, s)) = case mTpId of
        Nothing -> (DPathError p, [s])
        Just tpId -> (DTimePointError p tpId, [s])
    indexedErrs
       = .errs d
      ++ List.map indexDumErr dataErrs
      ++ List.map (\(p, s) -> (DPathError p, [s])) contErrs
  in ({types = newTm, postTypes = newPtm, tyAssns = newTam, nodes = newNm}, indexedErrs)

applyRootChanges : a -> Digest -> ByNs a -> ByNs a
applyRootChanges empty d bns =
  let
    applyRc s so = let ns = Tagged s in case so of
        SoAbsent -> CDict.remove ns
        SoPresentAfter _ -> CDict.update ns (Just << Maybe.withDefault empty)
  in Dict.foldl applyRc bns <| .rootCops d

applyDigest
   : Digest -> RemoteState
  -> (RemoteState, ByNs (List (DataErrorIndex, List String)))
applyDigest d rs =
  let
    nsCorrectedVss = applyRootChanges vsEmpty d rs
    applyNsd ns nsd (ars, aerrs) = case CDict.get ns ars of
        Nothing ->
            (ars, CDict.insert ns [(DGlobalError, ["Namespace missing from root"])] aerrs)
        Just vs -> let (newVs, es) = applyNsDigest nsd vs in
            (CDict.insert ns newVs ars, CDict.insert ns es aerrs)
    (appliedVss, errs) = CDict.foldl applyNsd (nsCorrectedVss, TD.empty) <| .nsds d
  in (appliedVss, errs)

-- To relay:

type SubOp
  = Subscribe
  | Unsubscribe

type alias TrcSubDigest =
  { postTypes : CmpDict (Namespace, Tagged PostDefinition Seg) (Seg, Seg) SubOp
  , types : CmpDict (Namespace, Tagged Definition Seg) (Seg, Seg) SubOp
  , data : CmpDict SubPath (Seg, Path) SubOp
  }

type alias CreateOp =
  { args : List (List (WireType, WireValue))
  , after : Maybe (Either Placeholder Seg)
  }

type alias Creates = Dict Path (CmpDict Placeholder Seg (Maybe Attributee, CreateOp))

ntsCmp : Cmp (Namespace, Tagged a Seg) (Seg, Seg)
ntsCmp =
  { toCmp = \(Tagged ns, Tagged s) -> (ns, s)
  , fromCmp = \(ns, s) -> (Tagged ns, Tagged s)
  }

type alias TrcUpdateDigest =
  { ns : Namespace
  , dd : DataDigest
  , creates : Creates
  , co : CmDigest (Either Placeholder Seg)
  }

type ToRelayDigest
  = Trcsd TrcSubDigest
  | Trcud TrcUpdateDigest
