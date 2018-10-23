module TimeSeries exposing
  ( TimeSeries, empty, nonEmpty, get, insert, remove, update, singleton, fold
  , times, map)

import Dict exposing (Dict)

import ClTypes exposing (TpId, Time)
import BiMap exposing (BiMap)

type alias TimeSeries a =
  { points : Dict Time a
  , tpIds : BiMap TpId Time
  }

empty : TimeSeries a
empty = {points = Dict.empty, tpIds = BiMap.empty}

nonEmpty : TimeSeries a -> Maybe (TimeSeries a)
nonEmpty a = if Dict.isEmpty <| .points a then Nothing else Just a

get : TpId -> TimeSeries a -> Maybe (Time, a)
get tpid {points, tpIds} =
  let
    getTap t = Maybe.map (\a -> (t, a)) <| Dict.get t points
  in Maybe.andThen getTap <| BiMap.getSec tpid tpIds

insert : TpId -> Time -> a -> TimeSeries a -> TimeSeries a
insert tpid t v {points, tpIds} =
  { points = Dict.insert t v points
  , tpIds = BiMap.insert tpid t tpIds
  }

remove : TpId -> TimeSeries a -> TimeSeries a
remove tpid {points, tpIds} =
  { points = case BiMap.getSec tpid tpIds of
        Nothing -> points
        Just t -> Dict.remove t points
  , tpIds = BiMap.remove tpid tpIds
  }

update : TpId -> (a -> a) -> TimeSeries a -> TimeSeries a
update tpid op ts = case get tpid ts of
    Nothing -> ts
    Just (t, a) -> insert tpid t (op a) ts

singleton : TpId -> Time -> a -> TimeSeries a
singleton tpid t a = insert tpid t a empty

withTpId : (Time -> TpId -> a -> b) -> BiMap TpId Time -> Time -> a -> b
withTpId f tpIds t a = case BiMap.getPri t tpIds of
    Just tpid -> f t tpid a
    -- This can't happen (due to the BiMap having a tpid for every time)
    -- but I can't express it in the type system so I have to make
    -- something up
    Nothing -> f t 0 a

fold : (Time -> TpId -> a -> acc -> acc) -> acc -> TimeSeries a -> acc
fold f acc {points, tpIds} = Dict.foldl (withTpId f tpIds) acc points

times : TimeSeries a -> List Time
times {points} = Dict.keys points

map : (Time -> TpId -> a -> b) -> TimeSeries a -> TimeSeries b
map f {points, tpIds} = {points = Dict.map (withTpId f tpIds) points, tpIds = tpIds}
