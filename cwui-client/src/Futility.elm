module Futility exposing (..)
-- Functional utility bits absent from elm std lib

-- Equivalent to mapM for Result:
mapAllFaily : (a -> Result x b) -> List a -> Result x (List b)
mapAllFaily act vs = List.foldl
    (\v -> Result.andThen
        (\acc -> Result.map (\b -> b :: acc) (act v))
        )
    (Ok [])
    vs
