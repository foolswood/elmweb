module SequenceOps exposing (SeqOp(..), applySeqOps, banish)

import Dict exposing (Dict)
import Set exposing (Set)
import Futility exposing (takeUntil)

type SeqOp a
  = SoPresentAfter (Maybe a)
  | SoAbsent

insertAfter : a -> a -> List a -> Result String (List a)
insertAfter tgt ref =
  let
    go acc remaining = case remaining of
        (v :: leftover) -> if v == ref
            then Ok <| acc ++ (v :: tgt :: leftover)
            else go (acc ++ [v]) leftover
        [] -> Err <| "Ref not present: " ++ toString ref
  in go []

applyOp : a -> SeqOp a -> List a -> Result String (List a)
applyOp tgt op l =
  let
    tgtRemoved = List.filter ((/=) tgt) l
  in case op of
    SoAbsent -> Ok tgtRemoved
    SoPresentAfter Nothing -> Ok <| tgt :: tgtRemoved
    SoPresentAfter (Just ref) -> insertAfter tgt ref tgtRemoved

applySeqOps : Dict comparable (SeqOp comparable) -> List comparable -> Result String (List comparable)
applySeqOps allOps initialList =
  let
    resolve ops resolved acc =
      let
        isResolved op = case op of
            SoPresentAfter (Just a) -> Set.member a resolved
            _ -> True
        (ready, unready) = Dict.partition (always isResolved) ops
        newAcc = acc ++ Dict.toList ready
        newResolved = Dict.foldl (\k _ -> Set.insert k) resolved ready
      in if Dict.isEmpty unready
        then Ok newAcc
        else if Dict.isEmpty ready
            then Err <| "Unable to resolve: " ++ toString unready
            else resolve unready newResolved newAcc
    initiallyResolved = Set.diff (Set.fromList initialList) (Set.fromList <| Dict.keys allOps)
    resolvedOps = resolve allOps initiallyResolved []
    applyOps = List.foldl (\(a, op) acc -> Result.andThen (\l -> applyOp a op l) acc) <| Ok initialList
  in Result.andThen applyOps resolvedOps

replacePrev : a -> Maybe a -> SeqOp a -> SeqOp a
replacePrev v prev op = case op of
    SoPresentAfter (Just v) -> SoPresentAfter prev
    _ -> op

banish : List comparable -> comparable -> Dict comparable (SeqOp comparable) -> Dict comparable (SeqOp comparable)
banish initialList v ops = case Dict.get v ops of
    Just (SoPresentAfter prev) -> Dict.map (always <| replacePrev v prev) <| Dict.remove v ops
    Just SoAbsent -> ops
    Nothing ->
        let
            itemBefore = List.head <| takeUntil ((==) v) initialList
        in Dict.insert v SoAbsent <| Dict.map (always <| replacePrev v itemBefore) ops
