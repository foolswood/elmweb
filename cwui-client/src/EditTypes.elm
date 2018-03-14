module EditTypes exposing (..)

import Dict exposing (Dict)

import Futility exposing (Conv)
import ClTypes exposing (WireValue, Seg)

type NeChildMod
  = NcmPresentAfter Seg
  | NcmAbsent

type alias NaConstT = List WireValue
type alias NaChildrenT = Dict Seg NeChildMod

type NodeActions
  = NaConst NaConstT
  | NaChildren NaChildrenT

constNaConv : Conv NodeActions NaConstT
constNaConv =
  let
    asNaConst na = case na of
        NaConst nac -> Ok nac
        _ -> Err "Not NaConst"
  in {wrap = NaConst, unwrap = asNaConst}

childrenNaConv : Conv NodeActions NaChildrenT
childrenNaConv =
  let
    asNaChildren na = case na of
        NaChildren nac -> Ok nac
        _ -> Err "Not NaChildren"
  in {wrap = NaChildren, unwrap = asNaChildren}

type alias NeChildState =
  { chosen : Bool
  , mod : Maybe NeChildMod
  }

type alias NeConstT = List (Maybe WireValue)
type alias NeChildrenT = Dict Seg NeChildState

type NodeEdit
  = NeConst NeConstT
  | NeChildren NeChildrenT

constNeConv : Conv NodeEdit NeConstT
constNeConv =
  let
    asNeConst edit = case edit of
        NeConst e -> Ok e
        _ -> Err "Not NeConst"
  in {wrap = NeConst, unwrap = asNeConst}

childrenNeConv : Conv NodeEdit NeChildrenT
childrenNeConv =
  let
    asNeChildren edit = case edit of
        NeChildren e -> Ok e
        _ -> Err "Not NeChildren"
  in {wrap = NeChildren, unwrap = asNeChildren}

type EditEvent es ss
  = EeUpdate es
  | EeSubmit ss

mapEe : (a -> b) -> (c -> d) -> EditEvent a c -> EditEvent b d
mapEe f g e = case e of
    EeUpdate p -> EeUpdate <| f p
    EeSubmit v -> EeSubmit <| g v
