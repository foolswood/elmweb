module EditTypes exposing (..)

import Dict exposing (Dict)

import ClTypes exposing (WireValue, Seg)

type alias NeConstT = List (Maybe WireValue)

type NodeEditEvent v
  = NeeUpdate v
  | NeeSubmit v

mapNee : (a -> b) -> NodeEditEvent a -> NodeEditEvent b
mapNee f e = case e of
    NeeUpdate v -> NeeUpdate <| f v
    NeeSubmit v -> NeeSubmit <| f v

type NeChildMod
  = NcmPresentAfter Seg
  | NcmAbsent

type alias NeChildState =
  { chosen : Bool
  , mod : Maybe NeChildMod
  }

type alias NeChildrenT = Dict Seg NeChildState

type NodeEdit
  = NeConst NeConstT
  | NeChildren NeChildrenT

asNeConst : NodeEdit -> Result String NeConstT
asNeConst edit = case edit of
    NeConst e -> Ok e
    _ -> Err "Not NeConst"

asNeChildren : NodeEdit -> Result String NeChildrenT
asNeChildren edit = case edit of
    NeChildren e -> Ok e
    _ -> Err "Not NeChildren"
