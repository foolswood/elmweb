module HtmlHelpers exposing (listEdit)
import Html as H exposing (Html)

listEdit : (a -> b) -> (a -> Html b) -> List a -> List (Html (List b))
listEdit toB f =
  let
    go pre l = case l of
        a :: rest ->
            (H.map (\newB -> pre ++ (newB :: List.map toB rest)) <| f a)
            :: go (pre ++ [toB a]) rest
        [] -> []
  in go []
