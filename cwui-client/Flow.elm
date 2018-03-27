import Dict exposing (Dict)
import Set exposing (Set)
import Json.Decode as JD

import Html as H
import Html.Events as HE

import Svg as S
import Svg.Attributes as SA
import Svg.Events as SE

main = H.beginnerProgram
  { model = exampleFlow
  , view = viewFlow
  , update = updateFlow
  }

exampleFlow =
  { dragging = Nothing
  , circles = Dict.fromList
    [ ("a", {x = 70, y = 100})
    , ("b", {x = 20, y = 30})
    , ("c", {x = 100, y = 60})
    ]
  , lines = Set.fromList
    [ ("a", "b")
    , ("b", "c")
    ]
  }

type FlowEvent
  = StartDrag String
  | StopDrag
  | Dragging Int Int
  | DoSodAll

mouseMove : (Int -> Int -> evt) -> S.Attribute evt
mouseMove e =
  let
    d = JD.map2 e (JD.field "clientX" JD.int) (JD.field "clientY" JD.int)
  in SE.on "mousemove" d

preventDragging : H.Attribute FlowEvent
preventDragging = HE.onWithOptions "dragstart" {preventDefault = True, stopPropagation = True} <| JD.succeed DoSodAll

viewFlow m =
  let
    globalAttrs = case .dragging m of
      Just _ -> [mouseMove Dragging, SE.onMouseUp StopDrag, preventDragging, HE.onMouseLeave StopDrag]
      Nothing -> [preventDragging]
    draggyCircle k pos = S.circle
      [ SA.cx <| toString <| .x pos, SA.cy <| toString <| .y pos, SA.r "10"
      , SE.onMouseDown <| StartDrag k
      ]
      []
    circles = List.map (uncurry draggyCircle) <| Dict.toList <| .circles m
    addLine (startK, endK) acc = case (Dict.get startK <| .circles m, Dict.get endK <| .circles m) of
        (Just start, Just end) -> S.line [SA.strokeWidth "1", SA.stroke "black", SA.x1 <| toString <| .x start, SA.x2 <| toString <| .x end, SA.y1 <| toString <| .y start, SA.y2 <| toString <| .y end] [] :: acc
        _ -> acc
    lines = Set.foldl addLine [] <| .lines m
  in S.svg globalAttrs <| lines ++ circles

updateFlow e m = case e of
    StartDrag k -> {m | dragging = Just k}
    StopDrag -> {m | dragging = Nothing}
    Dragging x y -> case .dragging m of
        Nothing -> m
        Just k -> {m | circles = Dict.update k (always <| Just {x = x, y = y}) <| .circles m}
    DoSodAll -> m
