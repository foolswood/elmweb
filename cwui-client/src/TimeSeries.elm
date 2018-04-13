module TimeSeries exposing (TimeSeries, empty, insert, remove, singleton, fold, times)

import Dict exposing (Dict)

import ClTypes exposing (TpId, Time)

type alias TimeSeries a =
  { points : Dict Time a
  , tpIds : BiMap
  }

empty : TimeSeries a
empty = {points = Dict.empty, tpIds = emptyBiMap}

insert : TpId -> Time -> a -> TimeSeries a -> TimeSeries a
insert tpid t v {points, tpIds} =
  { points = Dict.insert t v points
  , tpIds = insertById tpid t tpIds
  }

remove : TpId -> TimeSeries a -> TimeSeries a
remove tpid {points, tpIds} =
  { points = case getById tpid tpIds of
        Nothing -> points
        Just t -> Dict.remove t points
  , tpIds = removeById tpid tpIds
  }

singleton : TpId -> Time -> a -> TimeSeries a
singleton tpid t a = insert tpid t a empty

fold : (Time -> TpId -> a -> acc -> acc) -> acc -> TimeSeries a -> acc
fold f acc {points, tpIds} =
  let
    withTpId t a = case getByTime t tpIds of
        Just tpid -> f t tpid a
        -- This can't happen (due to the BiMap having a tpid for every time)
        -- but I can't express it in the type system so I have to make
        -- something up
        Nothing -> f t 0 a
  in Dict.foldl withTpId acc points

times : TimeSeries a -> List Time
times {points} = Dict.keys points

-- You'd think this could be generic, but since you can't have 2 different
-- comparables, nope!
type BiMap = BiMap (Dict TpId Time) (Dict Time TpId)

emptyBiMap : BiMap
emptyBiMap = BiMap Dict.empty Dict.empty

insertById : TpId -> Time -> BiMap -> BiMap
insertById tpid t (BiMap byId byTime) =
  let
    newById = Dict.insert tpid t byId
    newByTime = Dict.insert t tpid <| case Dict.get tpid byId of
        Just oldT -> Dict.remove oldT byTime
        Nothing -> byTime
  in BiMap newById newByTime

removeById : TpId -> BiMap -> BiMap
removeById tpid (BiMap byId byTime) =
  let
    newById = Dict.remove tpid byId
    newByTime = case Dict.get tpid byId of
        Just oldT -> Dict.remove oldT byTime
        Nothing -> byTime
  in BiMap newById newByTime

getById : TpId -> BiMap -> Maybe Time
getById tpid (BiMap byId _) = Dict.get tpid byId

getByTime : Time -> BiMap -> Maybe TpId
getByTime t (BiMap _ byTime) = Dict.get t byTime
