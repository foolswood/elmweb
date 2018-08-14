module Tagged.Cmp exposing (Cmp, idCmp, CmpSet, singleton, insert, union, empty, fromList, toList, diff)

import Set exposing (Set)

type alias Cmp a comparable = {toCmp : a -> comparable, fromCmp : comparable -> a}

idCmp : Cmp comparable comparable
idCmp = {toCmp = identity, fromCmp = identity}

type CmpSet a comparable = CmpSet (Cmp a comparable) (Set comparable)

empty : Cmp a comparable -> CmpSet a comparable
empty cmp = CmpSet cmp Set.empty

singleton : Cmp a comparable -> a -> CmpSet a comparable
singleton cmp a = CmpSet cmp <| Set.singleton <| .toCmp cmp a

insert : a -> CmpSet a comparable -> CmpSet a comparable
insert a (CmpSet cmp s) = CmpSet cmp <| Set.insert (.toCmp cmp a) s

union : CmpSet a comparable -> CmpSet a comparable -> CmpSet a comparable
union (CmpSet cmp a) (CmpSet _ b) = CmpSet cmp <| Set.union a b

diff : CmpSet a comparable -> CmpSet a comparable -> CmpSet a comparable
diff (CmpSet cmp a) (CmpSet _ b) = CmpSet cmp <| Set.diff a b

fromList : Cmp a comparable -> List a -> CmpSet a comparable
fromList cmp items = CmpSet cmp <| Set.fromList <| List.map (.toCmp cmp) items

toList : CmpSet a comparable -> List a
toList (CmpSet cmp s) = List.map (.fromCmp cmp) <| Set.toList s
