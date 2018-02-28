module Futility exposing (..)
-- Functional utility bits absent from elm std lib
import Dict exposing (Dict)

-- Equivalent to mapM for Result:
mapAllFaily : (a -> Result x b) -> List a -> Result x (List b)
mapAllFaily act vs = List.foldr
    (\v -> Result.andThen
        (\acc -> Result.map (\b -> b :: acc) (act v))
        )
    (Ok [])
    vs

itemAtIndex : Int -> List a -> Maybe a
itemAtIndex idx l = List.head (List.drop idx l)

replaceIdx : Int -> a -> List a -> Result String (List a)
replaceIdx idx v l = if List.length l > idx
  then Ok <| List.take idx l ++ v :: List.drop (idx + 1) l
  else Err "Index out of range"

updateIdx : (a -> Result String a) -> Int -> List a -> Result String (List a)
updateIdx f idx l =
    Result.map ((++) <| List.take idx l) <| case List.drop idx l of
        (oldA :: leftOver) -> Result.map (\newA -> newA :: leftOver) <| f oldA
        [] -> Err "Index out of range"

zip : List a -> List b -> List (a, b)
zip = List.map2 (,)

firstMatching : (a -> Bool) -> List a -> Maybe a
firstMatching pred l = case l of
    [] -> Nothing
    item :: rl -> if pred item then Just item else firstMatching pred rl

dropKeys : List comparable -> Dict comparable v -> Dict comparable v
dropKeys l = case l of
    [] -> identity
    (k :: ks) -> Dict.remove k << dropKeys ks
