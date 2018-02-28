module ClNodes exposing (..)

import Dict exposing (Dict)

import ClTypes exposing (Seg, Attributee, WireValue, WireType, TpId, Time, Interpolation)

type alias ConstData = (Maybe Attributee, List WireValue)

type alias TimePoint =
  { time : Time
  , attributee : Maybe Attributee
  , wvs : List WireValue
  , interpolation : Interpolation
  }
type alias TimeSeries a = Dict TpId a

type alias ConstDataNodeT =
  { types : List WireType
  , values : ConstData
  }

type alias TimeSeriesNodeT =
  { types : List WireType
  , values : TimeSeries TimePoint
  , removed : Dict TpId (Maybe Attributee)
  }

type alias ContainerNodeT = List Seg

type Node
  = ConstDataNode ConstDataNodeT
  | TimeSeriesNode TimeSeriesNodeT
  | ContainerNode ContainerNodeT

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
      { time = t
      , attributee = ma
      , wvs = wvs
      , interpolation = i
      }
  in case mn of
    Nothing -> Ok <| TimeSeriesNode
        { types = wts
        , values = Dict.singleton tpid tp
        , removed = Dict.empty
        }
    Just n -> case n of
        TimeSeriesNode tsn ->
          if .types tsn == wts
            then Ok <| TimeSeriesNode
                {tsn | values = Dict.insert tpid tp <| .values tsn}
          else Err "TimePoint WireType differs"
        _ -> Err "TimePoint set on non-series"

removeTimePoint : TpId -> Maybe Attributee -> Maybe Node -> Result String Node
removeTimePoint tpid ma mn = case mn of
    Nothing -> Err "Time point remove on missing node"
    Just n -> case n of
        TimeSeriesNode tsn -> Ok <| TimeSeriesNode
          { tsn
          | values = Dict.remove tpid <| .values tsn
          , removed = Dict.insert tpid ma <| .removed tsn
          }
        _ -> Err "Attempted to remove timepoint from non-series"

-- FIXME: The attributee is currently dropped in the child* functions

childPresentAfter : Maybe Attributee -> Seg -> Maybe Seg -> Maybe Node -> Result String Node
childPresentAfter att tgt mRef mn =
  let
    insertAfter ref acc remaining = case remaining of
        (v :: leftover) -> if v == ref
            then Ok <| acc ++ (tgt :: remaining)
            else insertAfter ref (acc ++ [v]) leftover
        [] -> Err <| "Ref not present: " ++ ref
    tgtRemoved = List.filter ((/=) tgt)
    presentAfter existing = case mRef of
        Nothing -> Ok <| tgt :: tgtRemoved existing
        Just ref -> insertAfter ref [] existing
  in case mn of
    Nothing -> Result.map ContainerNode <| presentAfter []
    Just n -> case n of
        ContainerNode kids -> Result.map ContainerNode <| presentAfter kids
        _ -> Err "Child present applied to non-container"

childAbsent : Maybe Attributee -> Seg -> Maybe Node -> Result String Node
childAbsent att tgt mn = case mn of
    Nothing -> Err "Child remove on missing node"
    Just n -> case n of
        ContainerNode children -> Ok <| ContainerNode <| List.filter ((/=) tgt) children
        _ -> Err "Attempted to remove child from non-container"
