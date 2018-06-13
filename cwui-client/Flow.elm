import Dict exposing (Dict)
import Set exposing (Set)
import Json.Decode as JD

import Html as H
import Html.Attributes as HA
import Html.Events as HE

import Svg as S
import Svg.Attributes as SA
import Svg.Events as SE

import Futility exposing (unionSets, keysSet, maybeToList)
import ClTypes exposing (Attributee)
import Digraph exposing (Connections)
import Transience exposing (Transience(..))

main = H.beginnerProgram
  { model = exampleFlow
  , view = viewFlow
  , update = updateFlow
  }

type alias PortId = String
type alias ElemId = String
type alias TermId = (ElemId, PortId)

type DragStartElem a
  = DragFromInput a
  | DragFromOutput a

mapDSE : (a -> b) -> DragStartElem a -> DragStartElem b
mapDSE f e = case e of
    DragFromInput a -> DragFromInput <| f a
    DragFromOutput a -> DragFromOutput <| f a

type alias Pos = (Int, Int)

type alias DragData =
  { start : DragStartElem TermId
  , pos : Pos
  , end : Maybe TermId
  }

type alias Element =
  { desc : String
  , attributee : Maybe Attributee
  , transience : Transience
  -- FIXME: Are these assoc lists?
  , inputs : Dict PortId String
  , outputs : Dict PortId {desc : String, cons : Dict TermId (Transience, Maybe Attributee)}
  }

nextElems : Element -> Set ElemId
nextElems {outputs} = Set.fromList <| List.concatMap (List.map Tuple.first << Dict.keys << .cons) <| Dict.values outputs

elemCons : Dict ElemId Element -> {fwd : Connections ElemId, rev : Connections ElemId}
elemCons allElems =
  let
    fwd = Dict.map (always nextElems) allElems
  in {fwd = fwd, rev = Digraph.reverseConnections fwd}

allowed : (ElemId -> Maybe (Set ElemId)) -> (Element -> Set PortId) -> ElemId -> Dict ElemId Element -> Set TermId
allowed next ports targetEid allElems =
  let
    getLoopy next eid = Set.insert eid <| case next eid of
        Nothing -> Set.empty
        Just n -> unionSets <| List.map (getLoopy next) <| Set.toList n
    loopyElems = getLoopy next targetEid
    asTerms eid = Set.map (\pid -> (eid, pid))
    termsFor eid e acc = if Set.member eid loopyElems
        then acc
        else Set.union acc <| asTerms eid <| ports e
  in Dict.foldl termsFor Set.empty allElems

allowedOuts : TermId -> Dict ElemId Element -> Set TermId
allowedOuts (inEid, _) allElems =
  allowed (\eid -> Maybe.map nextElems <| Dict.get eid allElems) (keysSet << .outputs) inEid allElems

allowedIns : TermId -> Dict ElemId Element -> Set TermId
allowedIns (outEid, _) allElems =
  let
    {rev} = elemCons allElems
  in allowed (\eid -> Dict.get eid rev) (keysSet << .inputs) outEid allElems

type alias FlowModel =
  { dragging : Maybe DragData
  , elements : Dict ElemId Element
  , pending : Dict (TermId, TermId) Bool
  }

exampleFlow : FlowModel
exampleFlow =
  let
    titoe desc cons =
      { desc = desc
      , attributee = Nothing
      , transience = TSteady
      , inputs = Dict.fromList
        [ ("a", "A")
        , ("b", "B")
        ]
      , outputs = Dict.fromList
        [ ("c", {desc = "C", cons = Dict.fromList <| List.map (\c -> (c, (TSteady, Nothing))) cons})
        , ("d", {desc = "D", cons = Dict.empty})
        ]
      }
  in
    { dragging = Nothing
    , elements = Dict.fromList
      [ ("e0", titoe "Eye Eye Eye Eye" [("e1", "b"), ("e2", "b")])
      , ("e1", titoe "Patch" [])
      , ("e2", titoe "Watch" [("e3", "a")])
      , ("e3", titoe "Enter" [])
      ]
    , pending = Dict.empty
    }

type FlowEvent
  = StartDrag (DragStartElem TermId) Pos
  | StopDrag
  | Dragging Int Int
  | DragSetEnd (Maybe TermId)
  | DoSodAll

mouseMove : (Int -> Int -> evt) -> S.Attribute evt
mouseMove e =
  let
    d = JD.map2 e (JD.field "clientX" JD.int) (JD.field "clientY" JD.int)
  in SE.on "mousemove" d

preventDragging : H.Attribute FlowEvent
preventDragging = HE.onWithOptions "dragstart" {preventDefault = True, stopPropagation = True} <| JD.succeed DoSodAll

dragEnds : DragStartElem TermId -> FlowModel -> (Bool, Set TermId, Set TermId)
dragEnds start m =
  let
    areConnected (outEid, outPid) inK = case Dict.get outEid <| .elements m of
        Nothing -> False
        Just e -> case Dict.get outPid <| .outputs e of
            Nothing -> False
            Just {cons} -> Dict.member inK cons
  in case start of
    DragFromInput startK ->
      let
        conExists endK = areConnected endK startK
        (existing, possibleNew) = Set.partition conExists <| allowedOuts startK <| .elements m
      in (False, possibleNew, existing)
    DragFromOutput startK ->
      let
        conExists = areConnected startK
        (existing, possibleNew) = Set.partition conExists <| allowedIns startK <| .elements m
      in (True, possibleNew, existing)

type EndpointState
  = EsNormal
  | EsInactive
  | EsAdd
  | EsRemove

type PortEvent
  = PeDragStart (DragStartElem PortId) Pos
  | PeDragOver (Maybe PortId)

translate : Pos -> S.Attribute a
translate pos = SA.transform <| "translate" ++ toString pos

type alias ElemView = {size : Pos, hs : List (H.Html PortEvent), ins : Dict PortId Pos, outs : Dict PortId Pos}

viewElement
   : (PortId -> EndpointState) -> (PortId -> EndpointState) -> Element -> ElemView
viewElement inState outState e =
  let
    midSpaceWidth = 5
    conWidth = 40
    headHeight = 40
    headBorder = 4
    conHeight = 25
    firstConCenter = round <| headHeight + (conHeight / 2)
    maxCons = max (Dict.size <| .inputs e) (Dict.size <| .outputs e)
    totalWidth = midSpaceWidth + (conWidth * 2)
    totalHeight = firstConCenter + (round <| (toFloat maxCons - 0.5) * conHeight)
    dragTargetActions pid =
      [ SE.onMouseOver <| PeDragOver <| Just pid
      , SE.onMouseOut <| PeDragOver Nothing
      ]
    inputLoc idx = (0, firstConCenter + (conHeight * idx))
    input idx (pid, desc) =
      let
        pos = inputLoc idx
        (fillColour, evtAttrs) = case inState pid of
            EsInactive -> ("grey", [])
            EsNormal -> ("blue", [SE.onMouseDown <| PeDragStart (DragFromInput pid) pos])
            EsAdd -> ("green", dragTargetActions pid)
            EsRemove -> ("red", dragTargetActions pid)
        inputPath = SA.d <| String.join " "
          [ "m0 -10"
          , "a 10 10 0 0 0 0 20"
          , "h" ++ toString conWidth
          , "v-20"
          , "Z"
          ]
      in S.g (translate pos :: evtAttrs)
        [ S.path [inputPath, SA.fill fillColour] []
        , S.text_ [SA.fill "white", SA.dominantBaseline "middle", SA.style "user-select: none"] [S.text desc]]
    outputLoc idx = (totalWidth, firstConCenter + (conHeight * idx))
    output idx (pid, o) =
      let
        pos = outputLoc idx
        (fillColour, evtAttrs) = case outState pid of
            EsInactive -> ("grey", [])
            EsNormal -> ("blue", [SE.onMouseDown <| PeDragStart (DragFromOutput pid) pos])
            EsAdd -> ("green", dragTargetActions pid)
            EsRemove -> ("red", dragTargetActions pid)
        outputPath = SA.d <| String.join " "
          [ "m-" ++ toString conWidth ++ " 10"
          , "v-20"
          , "h" ++ toString conWidth
          , "a 10 10 0 0 1 0 20"
          , "Z"
          ]
      in S.g (translate pos :: evtAttrs)
        [ S.path [outputPath, SA.fill fillColour] []
        , S.text_ [SA.fill "white", SA.dominantBaseline "middle", SA.textAnchor "end"] [S.text <| .desc o]]
    bgColour = case .transience e of
        TSteady -> "lightgrey"
        TNew -> "lightgreen"
        TRemoved -> "pink"
    box = S.rect
      [ SA.width <| toString totalWidth, SA.height <| toString totalHeight, SA.fill bgColour, SA.stroke "black" ]
      []
    heading = S.foreignObject
      [ translate (headBorder//2,headBorder//2)
      , SA.width <| toString <| totalWidth - headBorder, SA.height <| toString <| headHeight - headBorder]
      [ H.div [HA.attribute "xmlns" "http://www.w3.org/1999/xhtml"] [H.input [HA.size 4, HA.type_ "text", HA.value <| .desc e] []] ]
    inHs = List.indexedMap input <| Dict.toList <| .inputs e
    outHs = List.indexedMap output <| Dict.toList <| .outputs e
    insPos = Dict.fromList <| List.indexedMap (\idx k -> (k, inputLoc idx)) <| Dict.keys <| .inputs e
    outsPos = Dict.fromList <| List.indexedMap (\idx k -> (k, outputLoc idx)) <| Dict.keys <| .outputs e
  in {size = (totalWidth, totalHeight), hs = box :: heading :: inHs ++ outHs, ins = insPos, outs = outsPos}

viewElements : (TermId -> EndpointState) -> (TermId -> EndpointState) -> Dict ElemId Element -> {lines: List (H.Html a), elems : List (H.Html FlowEvent), ins : Dict TermId Pos, outs : Dict TermId Pos}
viewElements inState outState elems =
  let
    elemViews = Dict.map (\eid e -> viewElement (\pid -> inState (eid, pid)) (\pid -> outState (eid, pid)) e) elems
    columnAssignments = Digraph.inChainLen <| .rev <| elemCons elems
    positionElemView : ElemId -> Int -> ElemView -> (Dict ElemId (Pos, ElemView), Dict Int Int) -> (Dict ElemId (Pos, ElemView), Dict Int Int)
    positionElemView eid col ev (acc, colOffsets) =
      let
        yOff = Maybe.withDefault 0 <| Dict.get col colOffsets
        newColOffsets = Dict.insert col (yOff + (Tuple.second <| .size ev) + 10) colOffsets
        pos = (col * 150, yOff)
      in (Dict.insert eid (pos, ev) acc, newColOffsets)
    positionedElemViews = Tuple.first <| Dict.merge
        (always <| always identity)
        positionElemView
        (always <| always identity)
        columnAssignments elemViews (Dict.empty, Dict.empty)
    bindPortEvt eid evt =
      let
        bindPid pid = (eid, pid)
      in case evt of
        PeDragStart dse pos ->
          let
            boundDSE = mapDSE bindPid dse
          in StartDrag boundDSE <| posOf boundDSE
        PeDragOver mPid -> DragSetEnd <| Maybe.map bindPid mPid
    hGroups = List.map
        (\(eid, (pos, ev)) -> S.map (bindPortEvt eid) <| S.g [SA.transform <| "translate" ++ toString pos] <| .hs ev)
        <| Dict.toList positionedElemViews
    tagTerm f eid ((ex, ey), e) acc = Dict.foldl (\pid (px, py) -> Dict.insert (eid, pid) (ex + px, ey + py)) acc <| f e
    tagTerms f = Dict.foldl (tagTerm f) Dict.empty positionedElemViews
    insPos = tagTerms .ins
    outsPos = tagTerms .outs
    posOf dse = Maybe.withDefault (0,0) <| case dse of
        DragFromInput tid -> Dict.get tid insPos
        DragFromOutput tid -> Dict.get tid outsPos
    oConLines eid (oid, o) =
      let
        opos = posOf <| DragFromOutput (eid, oid)
        endsAt (itid, x) = (opos, posOf <| DragFromInput itid, x)
      in List.map endsAt <| Dict.toList <| .cons o
    conLines (eid, e) = List.concatMap (oConLines eid) <| Dict.toList <| .outputs e
    lineEnds = List.concatMap conLines <| Dict.toList elems
    lineItem ((ax, ay), (bx, by), (trans, attr)) = S.line
        [ SA.x1 <| toString ax, SA.x2 <| toString bx, SA.y1 <| toString ay, SA.y2 <| toString by
        , SA.strokeWidth "2"
        , SA.stroke <| case trans of
            TSteady -> "black"
            TNew -> "darkgreen"
            TRemoved -> "darkred"
        ]
        []
    lines = List.map lineItem lineEnds
  in {lines = lines, elems = hGroups, ins = insPos, outs = outsPos}

viewFlow : FlowModel -> H.Html FlowEvent
viewFlow m =
  let
    {inputState, outputState, mDrag, globalAttrs} = case .dragging m of
        Nothing ->
          { inputState = always EsNormal
          , outputState = always EsNormal
          , mDrag = Nothing
          , globalAttrs = []
          }
        Just {start, pos, end} ->
          let
            (fromOutput, possibleNew, existing) = dragEnds start m
            dragLineState = case end of
                Nothing -> EsNormal
                Just endK -> keyState endK
            dragLineStyle = case dragLineState of
                EsNormal -> [SA.strokeWidth "1", SA.strokeDasharray "10,10", SA.stroke "blue"]
                EsAdd -> [SA.strokeWidth "2", SA.strokeDasharray "5,5", SA.stroke "green"]
                EsRemove -> [SA.strokeWidth "2", SA.strokeDasharray "5,5", SA.stroke "red"]
                EsInactive -> []
            asDragLine insPos outsPos =
              let
                mStartPos = case start of
                    DragFromInput startK -> Dict.get startK insPos
                    DragFromOutput startK -> Dict.get startK outsPos
                l (sx, sy) = flip S.line [] <|
                    [ SA.x1 <| toString <| Tuple.first pos, SA.y1 <| toString <| Tuple.second pos
                    , SA.x2 <| toString sx, SA.y2 <| toString sy
                    ] ++ dragLineStyle
              in Maybe.map l mStartPos
            keyState k = case Dict.get (asConPair start k) <| .pending m of
                Just connected -> if connected then EsRemove else EsAdd
                Nothing -> if Set.member k possibleNew
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
            , mDrag = Just asDragLine
            , globalAttrs = [mouseMove Dragging, SE.onMouseUp StopDrag, preventDragging, HE.onMouseLeave StopDrag]
            }
    {lines, elems, ins, outs} = viewElements inputState outputState <| .elements m
    mDragLine = Maybe.andThen (\f -> f ins outs) mDrag
    pendLine (otid, itid) present acc = case (Dict.get otid outs, Dict.get itid ins) of
        (Just (ox, oy), Just (ix, iy)) -> S.line
          [ SA.x1 <| toString ox, SA.y1 <| toString oy, SA.x2 <| toString ix, SA.y2 <| toString iy
          , SA.stroke <| if present then "green" else "red"
          , SA.strokeWidth "1"
          ]
          [] :: acc
        _ -> acc
    pending = Dict.foldl pendLine [] <| .pending m
  in S.svg ([HA.width 1000, HA.height 500] ++ globalAttrs) <| lines ++ pending ++ maybeToList mDragLine ++ elems

asConPair : DragStartElem TermId -> TermId -> (TermId, TermId)
asConPair dse end = case dse of
    DragFromInput start -> (end, start)
    DragFromOutput start -> (start, end)

updateFlow : FlowEvent -> FlowModel -> FlowModel
updateFlow e m = case e of
    StartDrag dse pos -> {m | dragging = Just {pos = pos, start = dse, end = Nothing}}
    StopDrag ->
      let
        newPending = case .dragging m of
            Nothing -> .pending m
            Just {start, end} -> case end of
                Nothing -> .pending m
                Just endK -> togglePending <| asConPair start endK
        togglePending (outTid, inTid) = Dict.update (outTid, inTid) (toggleState <| currentState outTid inTid) <| .pending m
        toggleState current pending = case pending of
            Just present -> Just <| not present
            Nothing -> Just <| not current
        currentState (outEid, outPid) inTid = case Dict.get outEid <| .elements m of
            Nothing -> False
            Just e -> case Dict.get outPid <| .outputs e of
                Nothing -> False
                Just o -> Dict.member inTid <| .cons o
      in {m | dragging = Nothing, pending = newPending}
    DragSetEnd mk -> case .dragging m of
        Nothing -> m
        Just d -> {m | dragging = Just {d | end = mk}}
    Dragging x y -> case .dragging m of
        Nothing -> m
        Just d -> {m | dragging = Just {d | pos = (x, y)}}
    DoSodAll -> m
