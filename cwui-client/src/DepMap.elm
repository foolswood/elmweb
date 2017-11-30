module DepMap exposing (StringDeps, empty, addDependency, removeDependency, getDependency, getDependents)
import Dict exposing (..)
import Set exposing (..)

-- It would be nice if this could be polymorphic, but it can't because elm has
-- no type classes and comparable
type alias StringDeps = (Dict String String, Dict String (Set String))

empty : StringDeps
empty = (Dict.empty, Dict.empty)

getSet : Maybe (Set a) -> Set a
getSet ms = Maybe.withDefault Set.empty ms

addDependency : String -> String -> StringDeps -> StringDeps
addDependency a b (fwd, rev) =
  let
    addToMaybeSet v ms = Just (Set.insert v (getSet ms))
    newRev = Dict.update b (addToMaybeSet a) rev
  in
    (Dict.insert a b fwd, newRev)

emptyToNothing : Set a -> Maybe (Set a)
emptyToNothing s = if Set.isEmpty s then Nothing else Just s

removeDependency : String -> StringDeps -> StringDeps
removeDependency a (fwd, rev) =
  let
    rmFromSet ms = emptyToNothing (Set.remove a (getSet ms))
  in
    case Dict.get a fwd of
        Nothing -> (fwd, rev)
        (Just b) -> (Dict.remove a fwd, Dict.update b rmFromSet rev)

getDependency : String -> StringDeps -> Maybe String
getDependency k (fwd, _) = Dict.get k fwd

getDependents : String -> StringDeps -> Set String
getDependents k (_, rev) = Maybe.withDefault Set.empty (Dict.get k rev)
