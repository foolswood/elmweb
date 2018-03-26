module TimeSeriesView exposing (..)

import Dict
import Json.Decode as JD

import CSS exposing (emPx, keyFramed, applyKeyFramed)

import ClTypes exposing (TpId, Time, Attributee, WireValue, Interpolation(..), fromFloat, fromTime)
import ClNodes exposing (TimePoint)
import TimeSeries exposing (TimeSeries)
import EditTypes exposing (NeConstT)

import Html exposing (..)
import Html.Attributes as HA exposing (..)
import Html.Events exposing (..)

type alias Viewport =
  { top : Float
  , left : Float
  }

type TsMsg
  = VZoom Float
  | HZoom Float
  | SetViewport Viewport
  | PlayheadSet Time

type alias TsModel =
  { series : List (TimeSeries TimePoint)
  , vZoom : Float
  , hZoom : Float
  , viewport : Viewport
  , playheadPos : Time
  }

type alias TimePointEdit =
  { time : Time
  , value : NeConstT
  , interpolation : Interpolation
  }

exampleTimeSeries : TsModel
exampleTimeSeries =
  let
    asSeries = List.foldl (\(tpid, t, tp) -> TimeSeries.insert tpid t tp) TimeSeries.empty
  in
  { series = List.map asSeries <|
    [ [ (21, (0, 0), TimePoint Nothing [] ILinear)
      , (22, (50, 0), TimePoint Nothing [] ILinear)
      , (23, (200, 0), TimePoint Nothing [] ILinear)
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
  }

getHeights : Float -> List a -> List Float
getHeights z ots = List.repeat (List.length ots) z

gridStyle = ("display", "grid")

toEm : Float -> String
toEm f = toString f ++ "em"

mapEm : List Float -> String
mapEm = String.join " " << List.map toEm

viewTimeSeries : TsModel -> Html TsMsg
viewTimeSeries s =
  let
    controlsHeight = 2.0
    labelWidth = 8.0
    scaleControl = div
      [style
        [ ("height", toEm controlsHeight), ("width", toEm labelWidth)
        , ("position", "sticky"), ("left", "0px"), ("top", "0px")
        , ("z-index", "4"), ("background", "lightgray")
        ]]
      [ Html.map HZoom <| viewTimeScale <| .hZoom s
      , Html.map VZoom <| viewTimeScale <| .vZoom s
      ]
    ticks = Html.map PlayheadSet <| viewTicks controlsHeight labelWidth (.hZoom s) (.left <| .viewport s) (250, 0)
    controls = div [] [scaleControl, ticks]
    seriesHeights = getHeights (.vZoom s) <| .series s
    totalHeight = controlsHeight + List.sum seriesHeights
    rowStyles =
      [ gridStyle
      , ("grid-template-rows", mapEm seriesHeights)
      ]
    dgStyles = rowStyles ++
      [ ("position", "absolute")
      , ("left", toEm labelWidth)
      , ("top", toEm controlsHeight)
      ]
    dataGrid = div [style dgStyles] <| List.map (viewTsData <| .hZoom s) <| .series s
    labelGrid = div
      [ style <| rowStyles ++
        [ ("position", "sticky"), ("left", "0px"), ("width", toEm labelWidth)
        , ("background", "gray"), ("z-index", "3")
        ]]
      <| List.map viewTsLabel <| List.repeat (List.length <| .series s) "bob"
    playhead = viewPlayhead totalHeight labelWidth (.hZoom s) <| .playheadPos s
  in div
    [ style [("height", "200px"), ("overflow", "auto"), ("position", "relative")]
    , onScrollEm SetViewport
    ]
    [ticks, scaleControl, dataGrid, labelGrid, playhead]

viewTimeScale : Float -> Html Float
viewTimeScale s = input
  [ style [("width", "80%")]
  , type_ "range"
  , HA.min "1", HA.max "10", HA.step "0.05"
  , value <| toString s, onInput <| Maybe.withDefault s << Result.toMaybe << String.toFloat]
  []

onClickPosEm : (Float -> Float -> evt) -> Attribute evt
onClickPosEm e =
  let
    pd = JD.map (\v -> toFloat v / emPx ()) JD.int
  in on "click" <| JD.map2 e (JD.field "clientX" pd) (JD.field "clientY" pd)

onScrollEm : (Viewport -> evt) -> Attribute evt
onScrollEm e =
  let
    pd = JD.map (\v -> toFloat v / emPx ()) JD.int
  in on "scroll" <| JD.map2
    (\t l -> e <| Viewport t l)
    (JD.at ["target", "scrollTop"] pd)
    (JD.at ["target", "scrollLeft"] pd)

viewTicks : Float -> Float -> Float -> Float -> Time -> Html Time
viewTicks height leftMargin scale scrollOffset maxTime =
  let
    maxFloatTime = fromTime maxTime
    interval = 10.0
    nTicks = floor <| maxFloatTime / interval
    ticks = List.map (\tn -> interval * toFloat tn) <| List.range 0 nTicks
    viewTick t = div
      [style [("position", "absolute"), ("left", toString (scale * t) ++ "em")]]
      [text <| toString t]
  in div
    [style [("position", "sticky"), ("top", "0px"), ("z-index", "1"), ("height", "0px")]]
    [div
      [ style
          [ ("position", "relative"), ("left", toString leftMargin ++ "em")
          , ("background", "white"), ("height", toEm height)
          , ("width", toString ((maxFloatTime * scale) + 5) ++ "em")
          ]
      , onClickPosEm (\x _ -> fromFloat ((x + scrollOffset - leftMargin) / scale))
      ]
      <| List.map viewTick ticks]

viewTsData : Float -> TimeSeries TimePoint -> Html a
viewTsData scale ts =
  let
    lefts = List.map (((*) scale) << fromTime) <| TimeSeries.times ts
    colStyles =
      [ gridStyle
      , ("grid-template-columns", mapEm lefts ++ " 1fr")
      ]
    prePoint = div [] []
    contentGrid = div
      [style <| ("height", "100%") :: colStyles]
      <| prePoint :: TimeSeries.fold (\t tpid tp acc -> viewTimePoint tpid tp :: acc) [] ts
    popOvers = if True
      then [div
        [style
          [ ("position", "absolute"), ("top", "0px"), ("left", toEm <| 60 * scale)
          , ("background", "lightblue")]]
        [text "gubbins"]
        ]
      else []
  in div [style [("position", "relative")]] <| contentGrid :: popOvers

viewTimePoint : TpId -> TimePoint -> Html a
viewTimePoint _ _ = div [style [("border-left", "medium solid red"), ("background", "lightgreen")]] [text "foo"]

viewTsLabel : String -> Html a
viewTsLabel name = div [] [text name]

viewPlayhead : Float -> Float -> Float -> Time -> Html a
viewPlayhead height offset scale t =
  let
    left = (toEm <| offset + (scale * fromTime t))
    kfd = Dict.singleton "playing" <| Dict.fromList
      [ (0, Dict.singleton "left" left)
      , (100, Dict.singleton "left" <| toEm <| offset + (scale * (fromTime t + 2)))
      ]
    h = div
        [style
          [ ("position", "absolute"), ("top", "0px"), ("left", left)
          , ("height", toString height ++ "em"), ("width", "1px")
          , ("background", "green"), ("z-index", "2")
          , ("animation-timing-function", "linear")
          , ("animation-duration", "2s")
          , ("animation-name", "playing")]]
        []
    kf = keyFramed kfd h
  in applyKeyFramed kf

updateTimeSeries : TsMsg -> TsModel -> TsModel
updateTimeSeries evt m = case evt of
    VZoom z -> {m | vZoom = z}
    HZoom z -> {m | hZoom = z}
    SetViewport v -> {m | viewport = v}
    PlayheadSet t -> {m | playheadPos = t}
