module EditTypes exposing (..)

import Dict exposing (Dict)

import ClTypes exposing (WireValue, Seg)

type NeChildMod
  = NcmPresentAfter Seg
  | NcmAbsent

type alias NaConstT = List WireValue
type alias NaChildrenT = Dict Seg NeChildMod

type NodeActions
  = NaConst NaConstT
  | NaChildren NaChildrenT

type alias NeChildState =
  { chosen : Bool
  , mod : Maybe NeChildMod
  }

type alias NeConstT = List (Maybe WireValue)
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

type EditEvent es ss
  = EeUpdate es
  | EeSubmit ss

mapEe : (a -> b) -> (c -> d) -> EditEvent a c -> EditEvent b d
mapEe f g e = case e of
    EeUpdate p -> EeUpdate <| f p
    EeSubmit v -> EeSubmit <| g v
