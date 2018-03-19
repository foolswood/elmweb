module RemoteState exposing (TypeMap, TypeAssignMap, NodeMap, RemoteState, remoteStateEmpty)

import Dict exposing (Dict)

import ClTypes exposing (TypeName, Definition, Liberty, Path)
import ClNodes exposing (Node)

type alias TypeMap = Dict TypeName Definition
type alias TypeAssignMap = Dict Path (TypeName, Liberty)
type alias NodeMap = Dict Path Node

type alias RemoteState =
  { types : TypeMap
  , tyAssns : TypeAssignMap
  , nodes : NodeMap
  }

remoteStateEmpty : RemoteState
remoteStateEmpty = {types = Dict.empty, tyAssns = Dict.empty, nodes = Dict.empty}
