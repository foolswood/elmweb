module EditTypes exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)
import Regex exposing (Regex)

import Futility exposing (Conv)
import ClTypes exposing (WireValue, Seg, Interpolation, Time, AtomDef(..), WireValue(..), Bounds)
import SequenceOps exposing (SeqOp)
import TimeSeries exposing (TimeSeries)

type NaTimePoint
  = NatpSet
      { time : Time
      , interpolation : Interpolation
      , wvs : NaConstT
      }
  | NatpAbsent
type alias NeChildMod = SeqOp Seg

type alias NaConstT = List WireValue
type alias NaSeriesT = TimeSeries NaTimePoint
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

asFullTime : Bounds Time -> PartialTime -> Maybe Time
asFullTime _ pt = case pt of
    (Just s, Just f) -> Just (s, f)
    _ -> Nothing

asFull : (PartialEdit, AtomDef) -> Maybe WireValue
asFull ped = case ped of
    (PeEnum mi, ADEnum _) -> Maybe.map WvWord8 mi
    (PeTime pt, ADTime bs) -> Maybe.map WvTime <| asFullTime bs pt
    (PeString s, ADString (_, re)) -> case Regex.find (Regex.AtMost 1) re s of
        [] -> Nothing
        _ -> Just <| WvString s
    _ -> Nothing

asPartialTime : Bounds Time -> Maybe Time -> PartialTime
asPartialTime _ mt = case mt of
    Nothing -> (Nothing, Nothing)
    Just (s, f) -> (Just s, Just f)

asPartial : AtomDef -> Maybe WireValue -> PartialEdit
asPartial d mwv = case (d, mwv) of
    (ADEnum _, Just (WvWord8 w)) -> PeEnum <| Just w
    (ADEnum _, _) -> PeEnum Nothing
    (ADTime bs, Just (WvTime t)) -> PeTime <| asPartialTime bs <| Just t
    (ADTime bs, _) -> PeTime <| asPartialTime bs Nothing
    -- FIXME: This is utter tat!
    _ -> PeEnum Nothing

emptyPartial : List AtomDef -> List PartialEdit
emptyPartial = List.map (flip asPartial Nothing)

fullPartial : List AtomDef -> List WireValue -> List PartialEdit
fullPartial defs vs = List.map2 asPartial defs <| List.map Just vs

type alias PartialInterpolation = Maybe Interpolation  -- This won't be true once IBezier lands
type alias NeTimePoint =
  { time : PartialTime
  , interpolation : PartialInterpolation
  , wvs : NeConstT
  }

type alias NeConstT = List PartialEdit
type alias NeSeriesT = TimeSeries NeTimePoint
type alias NeChildrenT =
  { ops : Dict Seg (SeqOp Seg)
  , chosen : Set Seg
  , addSeg : String
  , dragging : Maybe (String, Maybe String)
  }

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
