import Html
import Dict

import TimeSeriesView exposing (..)
import ClTypes exposing (Interpolation(..), InterpolationLimit(..), WireValue(..), TupleDefinition)
import ClNodes exposing (TimePoint)
import TimeSeries
import Transience exposing (Transience(..))
import Form exposing (FormState(FsViewing))

exampleTimeSeries : TsModel
exampleTimeSeries =
  let
    asSeries = List.foldl (\(tpid, t, tp) -> TimeSeries.insert tpid t tp) TimeSeries.empty
    changedTimes = Dict.singleton (1, 0) <| Just (2, 0)
    asTsi pts =
      { path = toString pts
      , editable = False
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
  { series = List.map asTsi <|
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
  , vZoom = 2.0
  , hZoom = 1.0
  , viewport = Viewport 0 0
  , playheadPos = (42, 0)
  , selectedTps = Dict.empty
  }

main = Html.beginnerProgram
  { model = exampleTimeSeries
  , view = viewTimeSeries
  , update = updateTimeSeries
  }
