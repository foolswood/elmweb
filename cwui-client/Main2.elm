import Html
import Dict

import TimeSeriesView exposing (..)
import ClTypes exposing (Interpolation(..), InterpolationLimit(..), WireValue(..), TupleDefinition)
import ClNodes exposing (TimePoint)
import TimeSeries
import Transience exposing (Transience(..))
import Form exposing (FormState(FsViewing))
import TransportTracker exposing (Transport, TransportState(..))

exampleTimeSeries : TsModel
exampleTimeSeries =
  let
    asSeries = List.foldl (\(tpid, t, tp) -> TimeSeries.insert tpid t tp) TimeSeries.empty
    changedTimes = Dict.singleton (1, 0) <| Just (2, 0)
    asTsi pts =
      { path = toString pts
      , editable = True
      , def = TupleDefinition "yo" [] ILLinear
      , label = "bob"
      , transience = TSteady
      , series = asSeries pts
      , changedTimes = changedTimes
      }
    pi =
      { base = Nothing
      , recents = []
      , fs = FsViewing
      , mp = Nothing
      }
  in
  {tsModelEmpty | series = List.map asTsi <|
    [ [ (21, (0, 0), pi)
      , (22, (50, 0), pi)
      , (23, (200, 0), pi)
      ]
    , [ (33, (3, 0), pi)
      ]
    , [ (21, (5, 0), pi)
      ]
    , [ (12, (7, 0), pi)
      ]
    , [ (99, (9, 0), pi)
      ]
    ]
  }

type alias Model = {tsm : TsModel, transp : Transport}

updateTimeSeries : TsMsg -> Model -> Model
updateTimeSeries evt m = case processTimeSeriesEvent evt <| .tsm m of
    TsemUpdate tsm -> {m | tsm = tsm}
    TsemSeek t ->
      let
        oldTransp = .transp m
        transp = {oldTransp | pos = t}
      in {m | transp = transp}
    -- FIXME: Throws away submission event
    TsemPointChange _ _ _ -> m

main = Html.beginnerProgram
  { model = {tsm = exampleTimeSeries, transp = {pos = (0,0), state = TransportStopped, attributee = Nothing}}
  , view = \{tsm, transp} -> viewTimeSeries tsm transp
  , update = updateTimeSeries
  }
