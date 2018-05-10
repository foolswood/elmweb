module ClNodes exposing (..)

import Dict exposing (Dict)

import Futility exposing (Conv)
import SequenceOps exposing (SeqOp, applySeqOps)
import ClTypes exposing (Seg, Attributee, WireValue, WireType, TpId, Time, Interpolation)
import TimeSeries exposing (TimeSeries)

type alias ConstData = (Maybe Attributee, List WireValue)

type alias TimePoint =
  { attributee : Maybe Attributee
  , wvs : List WireValue
  , interpolation : Interpolation
  }

type alias ConstDataNodeT =
  { types : List WireType
  , values : ConstData
  }

type alias TimeSeriesNodeT =
  { types : List WireType
  , values : TimeSeries TimePoint
  }

type alias ContaineeT =
  { seg : Seg
  , attributee : Maybe Attributee
  }
type alias ContainerNodeT = List ContaineeT

type Node
  = ConstDataNode ConstDataNodeT
  | TimeSeriesNode TimeSeriesNodeT
  | ContainerNode ContainerNodeT

constNodeConv : Conv Node ConstDataNodeT
constNodeConv =
  let
    asConstDataNode n = case n of
        ConstDataNode cn -> Ok cn
        _ -> Err "Not a const node"
  in {wrap = ConstDataNode, unwrap = asConstDataNode}

seriesNodeConv : Conv Node TimeSeriesNodeT
seriesNodeConv =
  let
    asTimeSeriesNode n = case n of
        TimeSeriesNode tsn -> Ok tsn
        _ -> Err "Not timeseries"
  in {wrap = TimeSeriesNode, unwrap = asTimeSeriesNode}

childrenNodeConv : Conv Node ContainerNodeT
childrenNodeConv =
  let
    asContainerNode n = case n of
        ContainerNode cn -> Ok cn
        _ -> Err "Not container"
  in {wrap = ContainerNode, unwrap = asContainerNode}

setConstData : List WireType -> ConstData -> Maybe Node -> Result String Node
setConstData wts cd mn =
  let
    new = ConstDataNode {types = wts, values = cd}
  in case mn of
    Nothing -> Ok new
    Just n -> case n of
        ConstDataNode cdn ->
          if .types cdn == wts
          then Ok new
          else Err "Const node unexpectedly changed wire type"
        _ -> Err "Const set on non-const node"

setTimePoint
  :  List WireType -> TpId -> Time -> Maybe Attributee -> List WireValue
  -> Interpolation -> Maybe Node -> Result String Node
setTimePoint wts tpid t ma wvs i mn =
  let
    tp =
      { attributee = ma
      , wvs = wvs
      , interpolation = i
      }
  in case mn of
    Nothing -> Ok <| TimeSeriesNode
        { types = wts
        , values = TimeSeries.singleton tpid t tp
        }
    Just n -> case n of
        TimeSeriesNode tsn ->
          if .types tsn == wts
            then Ok <| TimeSeriesNode
                {tsn | values = TimeSeries.insert tpid t tp <| .values tsn}
          else Err "TimePoint WireType differs"
        _ -> Err "TimePoint set on non-series"

removeTimePoint : TpId -> Maybe Attributee -> Maybe Node -> Result String Node
removeTimePoint tpid ma mn = case mn of
    Nothing -> Err "Time point remove on missing node"
    Just n -> case n of
        TimeSeriesNode tsn -> Ok <| TimeSeriesNode
          { tsn
          | values = TimeSeries.remove tpid <| .values tsn
          }
        _ -> Err "Attempted to remove timepoint from non-series"

-- FIXME: Drops attributee
childUpdate : Dict Seg (Maybe Attributee, SeqOp Seg) -> Maybe Node -> Result String Node
childUpdate attOps mn =
  let
    asContainee = List.map (flip ContaineeT Nothing)
    ops = Dict.map (always Tuple.second) attOps
  in Result.map (ContainerNode << asContainee) <| case mn of
    Nothing -> applySeqOps ops []
    Just (ContainerNode kids) -> applySeqOps ops <| List.map .seg kids
    _ -> Err "Child present applied to non-container"
