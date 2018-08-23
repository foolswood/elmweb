module BiMap exposing (BiMap, empty, insert, remove, getPri, getSec)

import Dict exposing (Dict)

type BiMap comparable comparable1 =
    BiMap (Dict comparable comparable1) (Dict comparable1 comparable)

empty : BiMap comparable comparable1
empty = BiMap Dict.empty Dict.empty

insert
   : comparable -> comparable1
   -> BiMap comparable comparable1 -> BiMap comparable comparable1
insert pri sec (BiMap byPri bySec) =
  let
    newByPri = Dict.insert pri sec byPri
    newBySec = Dict.insert sec pri <| case Dict.get pri byPri of
        Just oldSec -> Dict.remove oldSec bySec
        Nothing -> bySec
  in BiMap newByPri newBySec

remove
   : comparable -> BiMap comparable comparable1 -> BiMap comparable comparable1
remove pri (BiMap byPri bySec) =
  let
    newById = Dict.remove pri byPri
    newByTime = case Dict.get pri byPri of
        Just oldSec -> Dict.remove oldSec bySec
        Nothing -> bySec
  in BiMap newById newByTime

getSec : comparable -> BiMap comparable comparable1 -> Maybe comparable1
getSec tpid (BiMap byPri _) = Dict.get tpid byPri

getPri : comparable1 -> BiMap comparable comparable1 -> Maybe comparable
getPri t (BiMap _ bySec) = Dict.get t bySec
