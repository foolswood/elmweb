import Html

import TimeSeriesView exposing (..)

main = Html.beginnerProgram
  { model = exampleTimeSeries
  , view = viewTimeSeries
  , update = updateTimeSeries
  }
