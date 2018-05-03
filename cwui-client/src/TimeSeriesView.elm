module TimeSeriesView exposing (..)

import Dict
import Json.Decode as JD

import CSS exposing (emPx, keyFramed, applyKeyFramed)

import ClTypes exposing
  ( TpId, Time, Attributee, WireValue, Interpolation(..), fromFloat, fromTime
  , TupleDefinition, InterpolationLimit(..), AtomDef(ADTime), unbounded)
import ClNodes exposing (TimePoint, TimeSeriesNodeT)
import TimeSeries exposing (TimeSeries)
import TimeSeriesDiff exposing (ChangedTimes)
import EditTypes exposing
  ( EditEvent(..), NeConstT, NeTimePoint, NaTimePoint(..), PartialTime
  , PartialInterpolation)
import Transience exposing (Transience(..))
import Form exposing (FormState(..), AtomState(AsEditing))
import Digests exposing (TimeChangeT, TimeSeriesDataOp(..))
import TupleViews
import Futility exposing (maybeToList, lastJust, setFst, setSnd)

import Html as H exposing (..)
import Html.Attributes as HA exposing (..)
import Html.Events as HE exposing (..)

type alias Viewport =
  { top : Float
  , left : Float
  }

type TsMsg
  = VZoom Float
  | HZoom Float
  | SetViewport Viewport
  | PlayheadSet Time

type alias SeriesInfo =
  { label : String
  , transience : Transience
  , series : TimeSeries TimePoint
  , changedTimes : ChangedTimes
  }

type alias TsModel =
  { series : List SeriesInfo
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
      [ H.map HZoom <| viewTimeScale <| .hZoom s
      , H.map VZoom <| viewTimeScale <| .vZoom s
      ]
    ticks = H.map PlayheadSet <| viewTicks controlsHeight labelWidth (.hZoom s) (.left <| .viewport s) (250, 0)
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
    dataGrid = div [style dgStyles] <| List.map (\si -> viewTsData (.hZoom s) (.series si) (.changedTimes si)) <| .series s
    labelGrid = div
      [ style <| rowStyles ++
        [ ("position", "sticky"), ("left", "0px"), ("width", toEm labelWidth)
        , ("background", "gray"), ("z-index", "3")
        ]]
      <| List.map (\si -> viewTsLabel (.label si) (.transience si)) <| .series s
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

viewTsData : Float -> TimeSeries TimePoint -> ChangedTimes -> Html a
viewTsData scale ts cts =
  let
    tFloat = (*) scale << fromTime
    lefts = List.map tFloat <| TimeSeries.times ts
    colStyles =
      [ gridStyle
      , ("grid-template-columns", mapEm lefts ++ " 1fr")
      ]
    prePoint = div [] []
    contentGrid = div
      [style <| ("height", "100%") :: colStyles]
      <| prePoint :: TimeSeries.fold (\t tpid tp acc -> viewTimePoint tpid tp :: acc) [] ts
    asHighlight start mDuration (hlStarts, prevEnd) = case mDuration of
        Nothing -> (toEm ((tFloat start) - prevEnd) :: "1fr" :: hlStarts, prevEnd)
        Just duration ->
          let
            fStart = tFloat start
            fDuration = tFloat duration
          in (toEm (fStart - prevEnd) :: toEm fDuration :: hlStarts, fStart + fDuration)
    (hlStarts, _) = Dict.foldr asHighlight ([], 0.0) cts
    highlightGrid = div
      [style
        [ gridStyle, ("position", "absolute"), ("top", "0px"), ("left", "0px"), ("height", "100%"), ("z-index", "-1")
        , ("grid-template-columns", String.join " " hlStarts)]
      ]
      <| List.map (\i -> div [style [("grid-column-start", toString <| 2 * (i + 1)), ("background", "purple")]] []) <| List.range 0 <| Dict.size cts - 1
    popOvers = if True
      then [div
        [style
          [ ("position", "absolute"), ("top", "0px"), ("left", toEm <| 60 * scale)
          , ("background", "lightblue")]]
        [text <| toString hlStarts]
        ]
      else []
  in div [style [("position", "relative")]] <| highlightGrid :: contentGrid :: popOvers

viewTimePoint : TpId -> TimePoint -> Html a
viewTimePoint _ _ = div [style [("border-left", "medium solid red"), ("background-color", "rgba(127, 255, 127, 0.7)")]] [text "foo"]

editTimePoint
   : TupleDefinition -> List TimeChangeT -> Maybe (Time, TimePoint)
  -> FormState NeTimePoint -> Maybe NaTimePoint
  -> Html (EditEvent NeTimePoint NaTimePoint)
editTimePoint def recents mBasePoint fs mp =
  let
    splitRecents (ma, tsdo) (_, recentValAcc, recentPtAcc) = case tsdo of
        OpSet t wts wvs i -> (False, recentValAcc ++ [(ma, wts, wvs)], recentPtAcc ++ [(ma, t, i)])
        OpRemove -> (True, recentValAcc, recentPtAcc)
    (removedUpstream, valueRecents, pointRecents) = List.foldl splitRecents (False, [], []) recents
    (valueBase, pointBase) = case mBasePoint of
        -- FIXME: Making up the types here because they're never used (so probably shouldn't flow everywhere)
        Just (t, {attributee, wvs, interpolation}) ->
          ( Just {types = [], values = (attributee, wvs)}
          , Just (attributee, t, interpolation))
        Nothing -> (Nothing, Nothing)
    (valueFs, pointFs) = case fs of
        FsViewing -> (FsViewing, FsViewing)
        FsEditing {wvs, time, interpolation} -> (FsEditing wvs, FsEditing (time, interpolation))
    (pendingDelete, valueMp, pointMp) = case mp of
        Just natp -> case natp of
            NatpSet {wvs, time, interpolation} -> (False, Just wvs, Just (time, interpolation))
            NatpAbsent -> (True, Nothing, Nothing)
        Nothing -> (False, Nothing, Nothing)
    ads = List.map Tuple.second <| .types def
    (valPartials, valSub) = TupleViews.pInfo ads valueBase valueRecents valueFs valueMp
    valView = TupleViews.viewWithRecentNoSubmission True ads valueRecents valueBase valPartials
    iLim = .interpLim def
    ptUpstream = mCurrentMeta pointBase pointRecents
    ptPartial = getPartialTimePointMeta iLim ptUpstream pointFs pointMp
    ptSub = asFullTimePointMeta ptPartial
    ptView = editTimePointMeta iLim ptUpstream ptPartial
    (partialTime, partialInterpolation) = ptPartial
    subBtn = case (valSub, ptSub) of
        (Just v, Just (t, i)) ->
          [ H.button [HE.onClick <| EeSubmit <|
              NatpSet {time = t, interpolation = i, wvs = v}] []
          ]
        _ -> []
    delBtn = if pendingDelete
        then []
        else [H.button [HE.onClick <| EeSubmit NatpAbsent] [H.text "Remove"]]
  in div [] <|
    [ H.map (\(pt, pi) -> EeUpdate {time = pt, interpolation = pi, wvs = valPartials}) ptView
    , H.map (\pwv -> EeUpdate {time = partialTime, interpolation = partialInterpolation, wvs = pwv}) valView
    ] ++ subBtn ++ delBtn

type alias TimePointMeta = (Maybe Attributee, Time, Interpolation)
type alias PartialTimePointMeta = (PartialTime, PartialInterpolation)
type alias PendingTimePointMeta = (Time, Interpolation)

mCurrentMeta : Maybe TimePointMeta -> List TimePointMeta -> Maybe TimePointMeta
mCurrentMeta base recents = List.head <| List.reverse <| maybeToList base ++ recents

asPendingTimePointMeta : TimePointMeta -> PendingTimePointMeta
asPendingTimePointMeta (_, t, i) = (t, i)

-- FIXME: Currently doesn't clamp to iLim
getPartialTimePointMeta
   : InterpolationLimit -> Maybe TimePointMeta
   -> FormState PartialTimePointMeta -> Maybe PendingTimePointMeta
   -> PartialTimePointMeta
getPartialTimePointMeta iLim mUpstream fs mp = case fs of
    FsViewing -> case lastJust (Maybe.map asPendingTimePointMeta mUpstream) mp of
        Just ((s, f), i) -> ((Just s, Just f), Just i)
        Nothing -> ((Nothing, Nothing), Nothing)
    FsEditing e -> e

asFullTimePointMeta : PartialTimePointMeta -> Maybe PendingTimePointMeta
asFullTimePointMeta p = case p of
    ((Just s, Just f), Just i) -> Just ((s, f), i)
    _ -> Nothing

editTimePointMeta
   : InterpolationLimit -> Maybe TimePointMeta
  -> PartialTimePointMeta
  -> Html PartialTimePointMeta
editTimePointMeta iLim upstream p = H.div []
    [ H.map (setFst p) <| TupleViews.timeEditor unbounded <| AsEditing <| Tuple.first p
    , H.map (setSnd p) <| interpolationEditor iLim <| Tuple.second p]

viewTimePointMeta : Maybe TimePointMeta -> List TimePointMeta -> Html a
viewTimePointMeta base recents = case mCurrentMeta base recents of
    Nothing -> H.text "Loading..."
    Just (ma, t, i) -> H.div [] [TupleViews.timeViewer unbounded t, interpolationViewer i]

interpolationViewer : Interpolation -> Html a
interpolationViewer = H.text << toString

interpolationEditor : InterpolationLimit -> PartialInterpolation -> Html PartialInterpolation
interpolationEditor il pi =
  let
    toInterp s = if s == "constant"
        then Just IConstant
        else if s == "linear"
            then Just ILinear
            else Nothing
    currentModeStr = case pi of
        Just IConstant -> "constant"
        Just ILinear -> "linear"
        Nothing -> ""
    modeOptStrs= case il of
        ILConstant -> ["constant"]
        ILLinear -> ["constant", "linear"]
        ILUninterpolated -> []
    modeOpt s = H.option [HA.value s, HA.selected <| currentModeStr == s] [H.text s]
  in H.select [onInput <| toInterp] <| List.map modeOpt modeOptStrs

viewTsLabel : String -> Transience -> Html a
viewTsLabel name transience =
  let
    attrs = case transience of
        TSteady -> []
        TNew -> [style [("border", "0.2em solid green")]]
        TRemoved -> [style [("border", "0.2em solid red")]]
  in div attrs [text name]

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
