import Html
import Dict

import TimeSeriesView exposing (..)
import ClTypes exposing (Interpolation(..), InterpolationLimit(..), WireValue(..), TupleDefinition)
import ClNodes exposing (TimePoint)
import TimeSeries
import Transience exposing (Transience(..))

exampleTimeSeries : TsModel
exampleTimeSeries =
  let
    asSeries = List.foldl (\(tpid, t, tp) -> TimeSeries.insert tpid t tp) TimeSeries.empty
    changedTimes = Dict.singleton (1, 0) <| Just (2, 0)
    asTsi pts =
      { path = toString pts
      , def = TupleDefinition "yo" [] ILLinear
      , label = "bob"
      , transience = TSteady
      , series = asSeries pts
      , changedTimes = changedTimes
      }
  in
  { series = List.map asTsi <|
    [ [ (21, (0, 0), TimePoint Nothing [WvInt32 1] ILinear)
      , (22, (50, 0), TimePoint Nothing [WvInt32 2] ILinear)
      , (23, (200, 0), TimePoint Nothing [WvInt32 3] ILinear)
      ]
    , [ (33, (3, 0), TimePoint Nothing [] ILinear)
      ]
    , [ (21, (5, 0), TimePoint Nothing [] ILinear)
      ]
    , [ (12, (7, 0), TimePoint Nothing [] ILinear)
      ]
    , [ (99, (9, 0), TimePoint Nothing [] ILinear)
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
