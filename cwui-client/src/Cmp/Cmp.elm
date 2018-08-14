module Cmp.Cmp exposing (Cmp, idCmp)

type alias Cmp a comparable = {toCmp : a -> comparable, fromCmp : comparable -> a}

idCmp : Cmp comparable comparable
idCmp = {toCmp = identity, fromCmp = identity}
