module Cmp.Dict exposing
  ( CmpDict, empty, singleton, insert, remove, get, getWithDefault, keys
  , values, foldl, update, map)

import Dict exposing (Dict)

import Cmp.Cmp exposing (Cmp)

type CmpDict k comparable v = CmpDict (Cmp k comparable) (Dict comparable v)

empty : Cmp k comparable -> CmpDict k comparable v
empty cmp = CmpDict cmp Dict.empty

singleton : Cmp k comparable -> k -> v -> CmpDict k comparable v
singleton cmp k v = CmpDict cmp <| Dict.singleton (.toCmp cmp k) v

insert : k -> v -> CmpDict k comparable v -> CmpDict k comparable v
insert k v (CmpDict cmp d) = CmpDict cmp <| Dict.insert (.toCmp cmp k) v d

remove : k -> CmpDict k comparable v -> CmpDict k comparable v
remove k (CmpDict cmp d) = CmpDict cmp <| Dict.remove (.toCmp cmp k) d

get : k -> CmpDict k comparable v -> Maybe v
get k (CmpDict cmp d) = Dict.get (.toCmp cmp k) d

getWithDefault : v -> k -> CmpDict k comparable v -> v
getWithDefault v k cd = Maybe.withDefault v <| get k cd

keys : CmpDict k comparable v -> List k
keys (CmpDict cmp d) = List.map (.fromCmp cmp) <| Dict.keys d

values : CmpDict k comparable v -> List v
values (CmpDict _ d) = Dict.values d

foldl : (k -> v -> a -> a) -> a -> CmpDict k comparable v -> a
foldl f initial (CmpDict cmp d) = Dict.foldl (f << .fromCmp cmp) initial d

update : k -> (Maybe v -> Maybe v) -> CmpDict k comparable v -> CmpDict k comparable v
update k f (CmpDict cmp d) = CmpDict cmp <| Dict.update (.toCmp cmp k) f d

map : (k -> a -> b) -> CmpDict k comparable a -> CmpDict k comparable b
map f (CmpDict cmp d) = CmpDict cmp <| Dict.map (f << .fromCmp cmp) d
