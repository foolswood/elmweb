module EditTypes exposing (..)

import Dict exposing (Dict)

import Futility exposing (Conv)
import ClTypes exposing (WireValue, Seg)
import SequenceOps exposing (SeqOp)

type alias NeChildMod = SeqOp Seg

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

type alias PartialTime = (Maybe Int, Maybe Int)

type PartialEdit
  = PeEnum (Maybe Int)
  | PeTime PartialTime
  | PeString String
  | PeFloat (Maybe Float)

pTimeConv : Conv PartialEdit PartialTime
pTimeConv =
  { wrap = PeTime
  , unwrap = \pe -> case pe of
    PeTime v -> Ok v
    _ -> Err "Not PeTime"
  }

pEnumConv : Conv PartialEdit (Maybe Int)
pEnumConv =
 { wrap = PeEnum
 , unwrap = \pe -> case pe of
    PeEnum v -> Ok v
    _ -> Err "Not PeEnum"
 }

pStringConv : Conv PartialEdit String
pStringConv =
  { wrap = PeString
  , unwrap = \pe -> case pe of
    PeString s -> Ok s
    _ -> Err "Not PeString"
  }

pFloatConv : Conv PartialEdit (Maybe Float)
pFloatConv =
  { wrap = PeFloat
  , unwrap = \pe -> case pe of
    PeFloat f -> Ok f
    _ -> Err "Not PeFloat"
  }

type alias NeConstT = List PartialEdit
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
