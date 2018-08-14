module Tagged.Dict exposing (TaggedDict, empty, singleton)

import Tagged.Tagged exposing (Tagged(..), tagCmp)
import Cmp.Dict as CmpDict exposing (CmpDict)

import Dict exposing (Dict)

type alias TaggedDict phantom comparable v = CmpDict (Tagged phantom comparable) comparable v

empty : TaggedDict phantom comparable v
empty = CmpDict.empty tagCmp

singleton : Tagged phantom comparable -> v -> TaggedDict phantom comparable v
singleton k v = CmpDict.singleton tagCmp k v
