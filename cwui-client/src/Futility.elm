module Futility exposing (..)
-- Functional utility bits absent from elm std lib

import Dict exposing (Dict)
import Set exposing (Set)
import Array exposing (Array)

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

-- FIXME: Would be better to use arrays for efficiency
replaceIdx : Int -> a -> List a -> Result String (List a)
replaceIdx idx v l = if List.length l > idx
  then Ok <| List.take idx l ++ v :: List.drop (idx + 1) l
  else Err "Index out of range"

updateIdx : (a -> Result String a) -> Int -> Array a -> Result String (Array a)
updateIdx f idx arr = case Array.get idx arr of
    Nothing -> Err "Index out of range"
    Just a -> Result.map (\newA -> Array.set idx newA arr) <| f a

zip : List a -> List b -> List (a, b)
zip = List.map2 (,)

firstMatching : (a -> Bool) -> List a -> Maybe a
firstMatching pred l = case l of
    [] -> Nothing
    item :: rl -> if pred item then Just item else firstMatching pred rl

castMaybe : (a -> Result String b) -> Maybe a -> Result String (Maybe b)
castMaybe c m = case m of
    Nothing -> Ok Nothing
    Just v -> Result.map Just <| c v

castList : (a -> Result String b) -> List a -> Result String (List b)
castList c l = case l of
    (a :: remainder) -> Result.map2 (::) (c a) (castList c remainder)
    [] -> Ok []

type alias Conv outer inner =
  { wrap : (inner -> outer)
  , unwrap : (outer -> Result String inner)
  }
