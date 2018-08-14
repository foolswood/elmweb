module Tagged.Dict exposing
  ( TaggedDict, empty, singleton, insert, remove, get, keys, values, foldl
  , update, getWithDefault, map)

import Tagged.Tagged exposing (Tagged(..))

import Dict exposing (Dict)

type TaggedDict phantom comparable v = TaggedDict (Dict comparable v)

empty : TaggedDict phantom comparable v
empty = TaggedDict Dict.empty

singleton : Tagged phantom comparable -> v -> TaggedDict phantom comparable v
singleton (Tagged k) v = TaggedDict <| Dict.singleton k v

insert
   : Tagged phantom comparable -> v -> TaggedDict phantom comparable v
  -> TaggedDict phantom comparable v
insert (Tagged k) v (TaggedDict d) = TaggedDict <| Dict.insert k v d

remove
   : Tagged phantom comparable -> TaggedDict phantom comparable v
  -> TaggedDict phantom comparable v
remove (Tagged k) (TaggedDict d) = TaggedDict <| Dict.remove k d

get : Tagged phantom comparable -> TaggedDict phantom comparable v -> Maybe v
get (Tagged k) (TaggedDict d) = Dict.get k d

keys : TaggedDict phantom comparable v -> List (Tagged phantom comparable)
keys (TaggedDict d) = List.map Tagged <| Dict.keys d

values : TaggedDict phantom comparable v -> List v
values (TaggedDict d) = Dict.values d

foldl : (Tagged phantom comparable -> v -> a -> a) -> a -> TaggedDict phantom comparable v -> a
foldl f initial (TaggedDict d) = Dict.foldl (\k -> f <| Tagged k) initial d

update
   : Tagged phantom comparable -> (Maybe v -> Maybe v)
  -> TaggedDict phantom comparable v -> TaggedDict phantom comparable v
update (Tagged k) f (TaggedDict d) = TaggedDict <| Dict.update k f d

getWithDefault
   : v -> Tagged phantom comparable -> TaggedDict phantom comparable v -> v
getWithDefault df (Tagged k) (TaggedDict d) =
    Maybe.withDefault df <| Dict.get k d

map
   : (Tagged phantom comparable -> v -> v1) -> TaggedDict phantom comparable v
  -> TaggedDict phantom comparable v1
map f (TaggedDict d) = TaggedDict <| Dict.map (f << Tagged) d
