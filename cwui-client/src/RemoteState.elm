module RemoteState exposing
  ( TypeMap, TypeAssignMap, NodeMap, RemoteState, remoteStateEmpty
  , remoteStateLookup, Valuespace, vsEmpty, unloadedPostTypes, ByNs)

import Dict exposing (Dict)
import Set exposing (Set)

import ClTypes exposing
  ( Definition(..), TupleDefinition, PostDefinition, Editable, Path, Namespace
  , Seg, TypeName)
import ClNodes exposing (Node)
import Tagged.Tagged as T exposing (Tagged)
import Tagged.Dict as TD exposing (TaggedDict)
import Tagged.Set as TS exposing (TaggedSet)

type alias TypeMap a = TaggedDict a Seg a
type alias TypeAssignMap = Dict Path (Tagged Definition Seg, Editable)
type alias NodeMap = Dict Path Node

type alias Valuespace =
  { types : TypeMap Definition
  , postTypes : TypeMap PostDefinition
  , tyAssns : TypeAssignMap
  , nodes : NodeMap
  }

vsEmpty : Valuespace
vsEmpty =
  { types = TD.empty, postTypes = TD.empty, tyAssns = Dict.empty
  , nodes = Dict.empty}

vsNode : Path -> Valuespace -> Result String Node
vsNode p vs = case Dict.get p <| .nodes vs of
    Nothing -> Err <| "Node not found for path: " ++ p
    Just n -> Ok n

vsTyAssn : Path -> Valuespace -> Result String (Tagged Definition Seg, Editable)
vsTyAssn p vs = case Dict.get p <| .tyAssns vs of
    Nothing -> Err <| "Type assignment not found for path: " ++ p
    Just tsl -> Ok tsl

vsTyDef : Path -> Valuespace -> Result String (Definition, Editable)
vsTyDef p vs = case vsTyAssn p vs of
    Err _ -> Err <| "Unable to determine TypeSeg for " ++ p
    Ok (ts, ed) -> case TD.get ts <| .types vs of
        Nothing -> Err <| "No def for " ++ toString ts
        Just def -> Ok (def, ed)

type Postability
  = PostableLoaded PostDefinition
  | PostableUnloaded (Tagged PostDefinition Seg)
  | Unpostable

vsPostability : Definition -> Valuespace -> Postability
vsPostability d vs = case d of
    ArrayDef {postType} -> case postType of
        Nothing -> Unpostable
        Just postSeg -> case TD.get postSeg <| .postTypes vs of
            Just postDef -> PostableLoaded postDef
            Nothing -> PostableUnloaded postSeg
    _ -> Unpostable

type alias ByNs a = Dict Namespace a
type alias RemoteState = ByNs Valuespace

remoteStateEmpty : RemoteState
remoteStateEmpty = Dict.empty

remoteStateLookup
   : Namespace -> Path -> RemoteState
  -> Result String (Node, Definition, Editable, Postability)
remoteStateLookup ns p rs = case Dict.get ns rs of
    Nothing -> Err <| "No info about the namespace: " ++ ns
    Just vs -> Result.map2 (\n (d, e) -> (n, d, e, vsPostability d vs))
        (vsNode p vs) (vsTyDef p vs)

unloadedPostTypes : RemoteState -> TaggedSet PostDefinition TypeName
unloadedPostTypes =
  let
    appendUnloaded ns vs def acc = case vsPostability def vs of
        (PostableUnloaded s) -> T.map ((,) ns) s :: acc
        _ -> acc
    ulpt ns vs acc = List.foldl (appendUnloaded ns vs) acc <| TD.values <| .types vs
  in TS.fromList << Dict.foldl ulpt []
