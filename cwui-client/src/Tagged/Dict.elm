module Tagged.Dict exposing
  ( TaggedDict, empty, insert, remove, get, values, foldl, update)

import Tagged.Tagged exposing (Tagged(..))

import Dict exposing (Dict)

type TaggedDict phantom comparable v = TaggedDict (Dict comparable v)

empty : TaggedDict phantom comparable v
empty = TaggedDict Dict.empty

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

values : TaggedDict phantom comparable v -> List v
values (TaggedDict d) = Dict.values d

foldl : (Tagged phantom comparable -> v -> a -> a) -> a -> TaggedDict phantom comparable v -> a
foldl f initial (TaggedDict d) = Dict.foldl (\k -> f <| Tagged k) initial d

update
   : Tagged phantom comparable -> (Maybe v -> Maybe v)
  -> TaggedDict phantom comparable v -> TaggedDict phantom comparable v
update (Tagged k) f (TaggedDict d) = TaggedDict <| Dict.update k f d
