module Futility exposing (..)
-- Functional utility bits absent from elm std lib

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

zip : List a -> List b -> List (a, b)
zip = List.map2 (,)

firstMatching : (a -> Bool) -> List a -> Maybe a
firstMatching pred l = case l of
    [] -> Nothing
    item :: rl -> if pred item then Just item else firstMatching pred rl

fst : (a, b) -> a
fst (a, b) = a

snd : (a , b) -> b
snd (a, b) = b
