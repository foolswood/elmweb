module RemoteState exposing
  ( TypeMap, TypeAssignMap, NodeMap, RemoteState, remoteStateEmpty
  , remoteStateLookup, Valuespace, vsEmpty, requiredPostTypes, ByNs
  , Postability(..), allTimeSeries)

import Dict exposing (Dict)
import Set exposing (Set)

import Futility exposing (mapMaybe)
import ClTypes exposing
  ( Definition(..), TupleDefinition, PostDefinition, Editable, Path, Namespace
  , Seg, TypeName, typeName, NsTag)
import ClNodes exposing (Node, TimeSeriesNodeT)
import Cmp.Dict as CDict
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
    Ok (ts, ed) -> case CDict.get ts <| .types vs of
        Nothing -> Err <| "No def for " ++ toString ts
        Just def -> Ok (def, ed)

type Postability
  = PostableLoaded (Tagged PostDefinition Seg) PostDefinition
  | PostableUnloaded (Tagged PostDefinition Seg)
  | Unpostable

vsPostability : Definition -> Valuespace -> Postability
vsPostability d vs = case d of
    ArrayDef {postType} -> case postType of
        Nothing -> Unpostable
        Just postSeg -> case CDict.get postSeg <| .postTypes vs of
            Just postDef -> PostableLoaded postSeg postDef
            Nothing -> PostableUnloaded postSeg
    _ -> Unpostable

type alias ByNs a = TaggedDict NsTag Seg a
type alias RemoteState = ByNs Valuespace

remoteStateEmpty : RemoteState
remoteStateEmpty = TD.empty

remoteStateLookup
   : Namespace -> Path -> RemoteState
  -> Result String (Node, Definition, Editable, Postability)
remoteStateLookup ns p rs = case CDict.get ns rs of
    Nothing -> Err <| "No info about: " ++ toString ns
    Just vs -> Result.map2 (\n (d, e) -> (n, d, e, vsPostability d vs))
        (vsNode p vs) (vsTyDef p vs)

requiredPostTypes : RemoteState -> TaggedSet PostDefinition TypeName
requiredPostTypes =
  let
    appendRequired ns vs def acc = case vsPostability def vs of
        (PostableLoaded s _) -> typeName ns s :: acc
        (PostableUnloaded s) -> typeName ns s :: acc
        _ -> acc
    ulpt ns vs acc = List.foldl (appendRequired ns vs) acc <| CDict.values <| .types vs
  in TS.fromList << CDict.foldl ulpt []

allTimeSeries
   : Namespace -> RemoteState
  -> List (Path, TimeSeriesNodeT, TupleDefinition, Editable)
allTimeSeries ns rs = case CDict.get ns rs of
    Nothing -> []
    Just vs ->
      let
        gatherInfo (p, n) = case n of
            ClNodes.TimeSeriesNode tsn -> case vsTyDef p vs of
              Ok (d, e) -> Just (p, n, d, e)
              Err _ -> Nothing
            _ -> Nothing
        tsns = mapMaybe gatherInfo <| Dict.toList <| .nodes vs
      in []
