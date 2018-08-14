module Tagged.Set exposing (TaggedSet, empty, fromList, toList, diff, foldl)

import Set exposing (Set)

import Tagged.Tagged exposing (Tagged(..))

type TaggedSet phantom comparable = TaggedSet (Set comparable)

empty : TaggedSet phantom comparable
empty = TaggedSet Set.empty

fromList : List (Tagged phantom comparable) -> TaggedSet phantom comparable
fromList = TaggedSet << Set.fromList << List.map (\(Tagged a) -> a)

toList : TaggedSet phantom comparable -> List (Tagged phantom comparable)
toList (TaggedSet s) = List.map Tagged <| Set.toList s

diff
   : TaggedSet phantom comparable -> TaggedSet phantom comparable
  -> TaggedSet phantom comparable
diff (TaggedSet a) (TaggedSet b) = TaggedSet <| Set.diff a b

foldl : (Tagged phantom comparable -> a -> a) -> a -> TaggedSet phantom comparable -> a
foldl f initial (TaggedSet s) = Set.foldl (f << Tagged) initial s
