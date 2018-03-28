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

type alias Pos = (Int, Int)

type DragStartElem
  = DragFromInput String
  | DragFromOutput String

type alias DragData =
  { start : DragStartElem
  , pos : Pos
  , end : Maybe String
  }

type alias Connection = (String, String)

type alias FlowModel =
  { dragging : Maybe DragData
  , inputs : Dict String Pos
  , outputs : Dict String Pos
  , connections : Set Connection
  }

exampleFlow : FlowModel
exampleFlow =
  { dragging = Nothing
  , inputs = Dict.fromList
    [ ("a", (70, 100))
    , ("b", (20, 30))
    ]
  , outputs = Dict.fromList
    [ ("o", (100, 30))
    , ("p", (20, 100))
    ]
  , connections = Set.fromList
    [ ("a", "o")
    ]
  }

type FlowEvent
  = StartDrag DragStartElem Pos
  | StopDrag
  | Dragging Int Int
  | DragSetEnd (Maybe String)
  | DoSodAll

mouseMove : (Int -> Int -> evt) -> S.Attribute evt
mouseMove e =
  let
    d = JD.map2 e (JD.field "clientX" JD.int) (JD.field "clientY" JD.int)
  in SE.on "mousemove" d

preventDragging : H.Attribute FlowEvent
preventDragging = HE.onWithOptions "dragstart" {preventDefault = True, stopPropagation = True} <| JD.succeed DoSodAll

keysSet : Dict comparable v -> Set comparable
keysSet = Set.fromList << Dict.keys

dragEnds : DragStartElem -> FlowModel -> (Bool, Set String, Set String)
dragEnds start m = case start of
    DragFromInput startK ->
      let
        conExists endK = Set.member (startK, endK) <| .connections m
        (existing, possibleNew) = Set.partition conExists <| keysSet <| .outputs m
      in (False, possibleNew, existing)
    DragFromOutput startK ->
      let
        conExists endK = Set.member (endK, startK) <| .connections m
        (existing, possibleNew) = Set.partition conExists <| keysSet <| .inputs m
      in (True, possibleNew, existing)

type EndpointState
  = EsNormal
  | EsInactive
  | EsAdd
  | EsRemove

maybeToList : Maybe a -> List a
maybeToList m = case m of
    Nothing -> []
    Just a -> [a]

viewFlow : FlowModel -> H.Html FlowEvent
viewFlow m =
  let
    {inputState, outputState, dragLine, globalAttrs} = case .dragging m of
        Nothing ->
          { inputState = always EsNormal
          , outputState = always EsNormal
          , dragLine = Nothing
          , globalAttrs = []
          }
        Just {start, pos, end} ->
          let
            (fromOutput, possibleNew, existing) = dragEnds start m
            mStartingElem = case start of
                DragFromInput k -> Dict.get k <| .inputs m
                DragFromOutput k -> Dict.get k <| .outputs m
            dragLineStyle = case end of
                Nothing -> [SA.strokeWidth "1", SA.strokeDasharray "10,10", SA.stroke "blue"]
                Just endK -> if Set.member endK possibleNew
                    then [SA.strokeWidth "2", SA.strokeDasharray "5,5", SA.stroke "green"]
                    else if Set.member endK existing
                        then [SA.strokeWidth "2", SA.strokeDasharray "5,5", SA.stroke "red"]
                        else [SA.strokeWidth "1", SA.strokeDasharray "10,10", SA.stroke "grey"]
            asDragLine (sx, sy) = flip S.line [] <|
                [ SA.x1 <| toString <| Tuple.first pos, SA.y1 <| toString <| Tuple.second pos
                , SA.x2 <| toString sx, SA.y2 <| toString sy
                ] ++ dragLineStyle
            keyState k = if Set.member k possibleNew
                then EsAdd
                else if Set.member k existing
                    then EsRemove
                    else EsInactive
            outputState k = if fromOutput
                then EsNormal
                else keyState k
            inputState k = if fromOutput
                then keyState k
                else EsNormal
          in
            { inputState = inputState
            , outputState = outputState
            , dragLine = Maybe.map asDragLine mStartingElem
            , globalAttrs = [mouseMove Dragging, SE.onMouseUp StopDrag, preventDragging, HE.onMouseLeave StopDrag]
            }
    dragTargetActions k =
      [ SE.onMouseOver <| DragSetEnd <| Just k
      , SE.onMouseOut <| DragSetEnd Nothing
      ]
    input es k (x, y) =
      let
        commonAttrs = [SA.cx <| toString x, SA.cy <| toString y, SA.r "10"]
        stateAttrs = case es of
            EsInactive -> [SA.fill "grey"]
            EsNormal -> [SA.fill "blue", SE.onMouseDown <| StartDrag (DragFromInput k) (x, y)]
            EsAdd -> SA.fill "green" :: dragTargetActions k
            EsRemove -> SA.fill "red" :: dragTargetActions k
      in S.circle (commonAttrs ++ stateAttrs) []
    output es k (x, y) =
      let
        commonAttrs = [SA.cx <| toString x, SA.cy <| toString y, SA.r "8"]
        stateAttrs = case es of
            EsInactive -> [SA.fill "grey"]
            EsNormal -> [SA.fill "blue", SE.onMouseDown <| StartDrag (DragFromOutput k) (x, y)]
            EsAdd -> SA.fill "green" :: dragTargetActions k
            EsRemove -> SA.fill "red" :: dragTargetActions k
      in S.circle (commonAttrs ++ stateAttrs) []
    inputs = List.map (\(k, v) -> input (inputState k) k v) <| Dict.toList <| .inputs m
    outputs = List.map (\(k, v) -> output (outputState k) k v) <| Dict.toList <| .outputs m
    addLine (startK, endK) acc = case (Dict.get startK <| .inputs m, Dict.get endK <| .outputs m) of
        (Just (sx, sy), Just (ex, ey)) -> S.line [SA.strokeWidth "2", SA.stroke "black", SA.x1 <| toString sx, SA.x2 <| toString ex, SA.y1 <| toString sy, SA.y2 <| toString ey] [] :: acc
        _ -> acc
    completeLines = Set.foldl addLine [] <| .connections m
  in S.svg globalAttrs <| completeLines ++ (maybeToList dragLine) ++ inputs ++ outputs

connectionFor : DragData -> Maybe Connection
connectionFor {start, end} = case end of
    Nothing -> Nothing
    Just endK -> case start of
        DragFromInput startK -> Just (startK, endK)
        DragFromOutput startK -> Just (endK, startK)

updateFlow : FlowEvent -> FlowModel -> FlowModel
updateFlow e m = case e of
    StartDrag dse pos -> {m | dragging = Just {pos = pos, start = dse, end = Nothing}}
    StopDrag ->
      let
        toggleCon con cons = if Set.member con cons
            then Set.remove con cons
            else Set.insert con cons
        conChange = Maybe.withDefault identity <| Maybe.andThen (Maybe.map toggleCon << connectionFor) <| .dragging m
      in {m | dragging = Nothing, connections = conChange <| .connections m}
    DragSetEnd mk -> case .dragging m of
        Nothing -> m
        Just d -> {m | dragging = Just {d | end = mk}}
    Dragging x y -> case .dragging m of
        Nothing -> m
        Just d -> {m | dragging = Just {d | pos = (x, y)}}
    DoSodAll -> m
