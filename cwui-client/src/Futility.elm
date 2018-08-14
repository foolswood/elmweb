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

-- Equivalent to mapM for Maybe:
allGood : (a -> Maybe b) -> List a -> Maybe (List b)
allGood f l = case l of
    [] -> Just []
    (x :: xs) -> Maybe.andThen (\y -> Maybe.map (\ys -> y :: ys) <| allGood f xs) <| f x

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

takeUntil : (a -> Bool) -> List a -> List a
takeUntil pred l = case l of
    [] -> []
    (a :: remaining) -> if pred a
        then []
        else a :: takeUntil pred remaining

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

type Either l r
  = Left l
  | Right r

either : (a -> c) -> (b -> c) -> Either a b -> c
either lf rf e = case e of
    Left a -> lf a
    Right c -> rf c

unionSets : List (Set comparable) -> Set comparable
unionSets = List.foldl Set.union Set.empty

keysSet : Dict comparable v -> Set comparable
keysSet = Set.fromList << Dict.keys

maybeToList : Maybe a -> List a
maybeToList m = case m of
    Nothing -> []
    Just a -> [a]

appendMaybe : Maybe a -> List a -> List a
appendMaybe ma l = Maybe.withDefault l <| Maybe.map (\a -> l ++ [a]) ma

dictMapMaybe : (comparable -> a -> Maybe b) -> Dict comparable a -> Dict comparable b
dictMapMaybe f =
  let
    insertWhenJust k a = case f k a of
        Just b -> Dict.insert k b
        Nothing -> identity
  in Dict.foldl insertWhenJust Dict.empty

removeKeys : List comparable -> Dict comparable v -> Dict comparable v
removeKeys ks d = List.foldl Dict.remove d ks

nonEmptyDict : Dict k v -> Maybe (Dict k v)
nonEmptyDict d = if Dict.isEmpty d then Nothing else Just d

last : List a -> Maybe a
last l = case l of
    [] -> Nothing
    (i :: []) -> Just i
    (_ :: c) -> last c

lastJust : Maybe a -> Maybe a -> Maybe a
lastJust mv mp = case mp of
    Just p -> Just p
    Nothing -> mv

setFst : (a, b) -> a -> (a, b)
setFst (_, b) a = (a, b)

setSnd : (a, b) -> b -> (a, b)
setSnd (a, _) b = (a, b)

getWithDefault : a -> comparable -> Dict comparable a -> a
getWithDefault df k d = Maybe.withDefault df <| Dict.get k d
