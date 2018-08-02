module RemoteState exposing (TypeMap, TypeAssignMap, NodeMap, RemoteState, remoteStateEmpty, tyDef, tyAssn)

import Dict exposing (Dict)

import ClTypes exposing (TypeName, Definition(..), PostDefinition, Editable, Path, Seg, ChildDescription)
import ClNodes exposing (Node)
import PathManipulation exposing (splitBasename)

type alias TypeMap a = Dict TypeName a
type alias TypeAssignMap = Dict Path (TypeName, Editable)
type alias NodeMap = Dict Path Node

type alias RemoteState =
  { types : TypeMap Definition
  , postTypes : TypeMap PostDefinition
  , tyAssns : TypeAssignMap
  , nodes : NodeMap
  }

remoteStateEmpty : RemoteState
remoteStateEmpty =
  { types = Dict.empty, postTypes = Dict.empty, tyAssns = Dict.empty
  , nodes = Dict.empty}

tyAssn : Path -> RemoteState -> Result String (TypeName, Editable)
tyAssn p rs = case Dict.get p <| .tyAssns rs of
    Nothing -> case splitBasename p of
        Nothing -> Err "Root not type assigned"
        Just (pp, cSeg) -> Result.andThen (childTypeName cSeg << Tuple.first) <| tyDef pp rs
    Just tnl -> Ok tnl

tyDef : Path -> RemoteState -> Result String (Definition, Editable)
tyDef p rs = case tyAssn p rs of
    Err _ -> Err <| "Unable to determine TypeName for " ++ p
    Ok (tn, ed) -> case Dict.get tn <| .types rs of
        Nothing -> Err <| "No def for " ++ toString tn
        Just def -> Ok (def, ed)

structChildTypeName : Seg -> List ChildDescription -> Result String (TypeName, Editable)
structChildTypeName s l = case l of
    ({name, typeRef, ed} :: xs) -> if name == s
        then Ok <| (typeRef, ed)
        else structChildTypeName s xs
    [] -> Err <| "Struct has no child: " ++ s

childTypeName : Seg -> Definition -> Result String (TypeName, Editable)
childTypeName s d = case d of
    StructDef {childDescs} -> structChildTypeName s childDescs
    ArrayDef {childType, childEditable} -> Ok (childType, childEditable)
    TupleDef _ -> Err "Tuples have no children"
