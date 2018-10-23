module Tagged.Set exposing (TaggedSet, empty, fromList, singleton)

import Cmp.Set as CmpSet exposing (CmpSet)
import Set exposing (Set)

import Tagged.Tagged exposing (Tagged, tagCmp)

type alias TaggedSet phantom comparable = CmpSet (Tagged phantom comparable) comparable

empty : TaggedSet phantom comparable
empty = CmpSet.empty tagCmp

fromList : List (Tagged phantom comparable) -> TaggedSet phantom comparable
fromList = CmpSet.fromList tagCmp

singleton : Tagged phantom comparable -> TaggedSet phantom comparable
singleton = CmpSet.singleton tagCmp
