module Tagged.Tagged exposing (Tagged(..), map)

type Tagged phantom a = Tagged a

map : (a -> b) -> Tagged phantom a -> Tagged phantom b
map f (Tagged a) = Tagged <| f a
