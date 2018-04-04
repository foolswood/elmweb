module Digraph exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)

type alias Connections a = Dict a (Set a)

reverseConnections : Connections comparable -> Connections comparable
reverseConnections fwd =
  let
    insertRevEntries k vs acc = Set.foldl (\v -> Dict.update v <| Just << Set.insert k << Maybe.withDefault Set.empty) acc vs
    initAcc = Dict.fromList <| List.map (\k -> (k, Set.empty)) <| Dict.keys fwd
  in Dict.foldl insertRevEntries initAcc fwd

inChainLen : Connections comparable -> Dict comparable Int
inChainLen =
  let
    inner i acc revCons =
      let
        (thisIterD, rawRemaining) = Dict.partition (always Set.isEmpty) revCons
        thisIterS = Set.fromList <| Dict.keys thisIterD
        thisIterUD = Dict.map (always <| always i) thisIterD
        filteredRemaining = Dict.map (\_ s -> Set.diff s thisIterS) rawRemaining
        newAcc = Dict.union thisIterUD acc
      in if Dict.isEmpty rawRemaining
        then newAcc
        else inner (i + 1) newAcc filteredRemaining
  in inner 0 Dict.empty
