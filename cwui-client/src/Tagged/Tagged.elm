module Tagged.Tagged exposing (Tagged(..), map, tagCmp)

import Cmp.Cmp exposing (Cmp)

type Tagged phantom a = Tagged a

map : (a -> b) -> Tagged phantom a -> Tagged phantom b
map f (Tagged a) = Tagged <| f a

tagCmp : Cmp (Tagged phantom comparable) comparable
tagCmp =
  { toCmp = (\(Tagged a) -> a)
  , fromCmp = Tagged
  }
