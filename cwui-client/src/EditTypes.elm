module EditTypes exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)
import Regex exposing (Regex)

import SequenceOps exposing (SeqOp)
import Cmp.Dict exposing (CmpDict)
import Futility exposing (Conv, Either)
import Tagged.Tagged exposing (Tagged)
import ClTypes exposing (WireValue, Seg, Interpolation, Time, AtomDef(..), WireValue(..), Bounds, Placeholder)
import TimeSeries exposing (TimeSeries)

-- FIXME: The source seg/id types kinda suck
type alias ChildSourceStateSeg = String
type alias DataSourceSeg = String

type alias ChildSourceStateId = List ChildSourceStateSeg
type alias DataSourceId = List DataSourceSeg

type NaTimePoint
  = NatpSet
      { time : Time
      , interpolation : Interpolation
      , wvs : NaConstT
      }
  | NatpAbsent

type alias NaConstT = List WireValue
type alias NaSeriesT = TimeSeries NaTimePoint
type NaChildrenT
  = NacCreate Placeholder (Maybe (Either Placeholder Seg)) (List (List WireValue))
  | NacMove Seg (Maybe Seg)
  | NacDelete Seg
  | NacSelect ChildSourceStateId Seg
  | NacDeselect ChildSourceStateId Seg

type NodeAction
  = NaConst NaConstT
  | NaSeries NaSeriesT
  | NaChildren NaChildrenT

constNaConv : Conv NodeAction NaConstT
constNaConv =
  let
    asNaConst na = case na of
        NaConst nac -> Ok nac
        _ -> Err "Not NaConst"
  in {wrap = NaConst, unwrap = asNaConst}

seriesNaConv : Conv NodeAction NaSeriesT
seriesNaConv =
  let
    asNaSeries na = case na of
        NaSeries nas -> Ok nas
        _ -> Err "Not NaSeries"
  in {wrap = NaSeries, unwrap = asNaSeries}

childrenNaConv : Conv NodeAction NaChildrenT
childrenNaConv =
  let
    asNaChildren na = case na of
        NaChildren nac -> Ok nac
        _ -> Err "Not NaChildren"
  in {wrap = NaChildren, unwrap = asNaChildren}

type alias PaChildrenT =
  { childMods : Dict Seg (SeqOp Seg)
  , creates : CmpDict Placeholder Seg (Maybe (Either Placeholder Seg), List (List WireValue))
  }

type PendingActions
  = PaConst NaConstT
  | PaSeries NaSeriesT
  | PaChildren PaChildrenT

constPaConv : Conv PendingActions NaConstT
constPaConv =
  let
    asPaConst pa = case pa of
        PaConst nac -> Ok nac
        _ -> Err "Not PaConst"
  in {wrap = PaConst, unwrap = asPaConst}

seriesPaConv : Conv PendingActions NaSeriesT
seriesPaConv =
  let
    asPaSeries pa = case pa of
        PaSeries nas -> Ok nas
        _ -> Err "Not PaSeries"
  in {wrap = PaSeries, unwrap = asPaSeries}

childrenPaConv : Conv PendingActions PaChildrenT
childrenPaConv =
  let
    asPaChildren pa = case pa of
        PaChildren nac -> Ok nac
        _ -> Err "Not PaChildren"
  in {wrap = PaChildren, unwrap = asPaChildren}

type alias PartialTime = (Maybe Int, Maybe Int)

type PartialEdit
  = PeEnum (Maybe Int)
  | PeTime PartialTime
  | PeString String
  | PeInt (Maybe Int)
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

pIntConv : Conv PartialEdit (Maybe Int)
pIntConv =
  { wrap = PeInt
  , unwrap = \pe -> case pe of
    PeInt i -> Ok i
    _ -> Err "Not PeInt"
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
    (PeEnum mi, ADEnum _) -> Maybe.map WvWord32 mi
    (PeTime pt, ADTime bs) -> Maybe.map WvTime <| asFullTime bs pt
    (PeInt mi, ADInt32 bs) -> Maybe.map WvInt32 mi
    (PeFloat mf, ADFloat bs) -> Maybe.map WvFloat mf
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
    (ADEnum _, Just (WvWord32 w)) -> PeEnum <| Just w
    (ADEnum _, _) -> PeEnum Nothing
    (ADTime bs, Just (WvTime t)) -> PeTime <| asPartialTime bs <| Just t
    (ADTime bs, _) -> PeTime <| asPartialTime bs Nothing
    (ADInt32 bs, Just (WvInt32 i)) -> PeInt <| Just i
    (ADInt32 bs, _) -> PeInt Nothing
    (ADFloat bs, Just (WvFloat f)) -> PeFloat <| Just f
    (ADFloat bs, _) -> PeFloat Nothing
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

type alias NeChildCreate =
  { desiredPlaceholder : String
  , ref : Maybe Seg
  , vals : List NeConstT
  }

type alias NeSeriesT = TimeSeries NeTimePoint

type alias NeChildrenT =
  { create : Maybe NeChildCreate
  , dragging : Maybe (String, Maybe String)
  }

type NodeEdit
  = NeConst NeConstT
  | NeSeries NeSeriesT
  | NeChildren NeChildrenT

constNeConv : Conv NodeEdit NeConstT
constNeConv =
  let
    asNeConst edit = case edit of
        NeConst e -> Ok e
        _ -> Err "Not NeConst"
  in {wrap = NeConst, unwrap = asNeConst}

seriesNeConv : Conv NodeEdit NeSeriesT
seriesNeConv =
  let
    asNeSeries edit = case edit of
        NeSeries e -> Ok e
        _ -> Err "Not NeSeries"
  in {wrap = NeSeries, unwrap = asNeSeries}

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
