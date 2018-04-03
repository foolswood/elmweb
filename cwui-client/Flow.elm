import Dict exposing (Dict)
import Set exposing (Set)
import Json.Decode as JD

import Html as H
import Html.Attributes as HA
import Html.Events as HE

import Svg as S
import Svg.Attributes as SA
import Svg.Events as SE

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
  -- FIXME: Are these assoc lists?
  , inputs : Dict PortId String
  , outputs : Dict PortId {desc : String, cons : Set TermId}
  }

unionSets : List (Set comparable) -> Set comparable
unionSets = List.foldl Set.union Set.empty

nextElems : Element -> Set ElemId
nextElems {outputs} = Set.fromList <| List.concatMap (List.map Tuple.first << Set.toList << .cons) <| Dict.values outputs

elemCons : Dict ElemId Element -> {fwd : Dict ElemId (Set ElemId), rev : Dict ElemId (Set ElemId)}
elemCons allElems =
  let
    fwd = Dict.map (always nextElems) allElems
    rev = Dict.foldl insertRevEntries Dict.empty fwd
    insertRevEntries k vs acc = Set.foldl (\v -> Dict.update v <| Just << Set.insert k << Maybe.withDefault Set.empty) acc vs
  in {fwd = fwd, rev = rev}

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
  }

exampleFlow : FlowModel
exampleFlow =
  let
    titoe desc cons =
      { desc = desc
      , inputs = Dict.fromList
        [ ("a", "A")
        , ("b", "B")
        ]
      , outputs = Dict.fromList
        [ ("c", {desc = "C", cons = Set.fromList cons})
        , ("d", {desc = "D", cons = Set.empty})
        ]
      }
  in
    { dragging = Nothing
    , elements = Dict.fromList
      [ ("e0", titoe "Eye Eye Eye Eye" [("e1", "b")])
      , ("e1", titoe "Patch" [])
      ]
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

keysSet : Dict comparable v -> Set comparable
keysSet = Set.fromList << Dict.keys

dragEnds : DragStartElem TermId -> FlowModel -> (Bool, Set TermId, Set TermId)
dragEnds start m =
  let
    areConnected (outEid, outPid) inK = case Dict.get outEid <| .elements m of
        Nothing -> False
        Just e -> case Dict.get outPid <| .outputs e of
            Nothing -> False
            Just {cons} -> Set.member inK cons
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

maybeToList : Maybe a -> List a
maybeToList m = case m of
    Nothing -> []
    Just a -> [a]

type PortEvent
  = PeDragStart (DragStartElem PortId) Pos
  | PeDragOver (Maybe PortId)

viewElement
   : (PortId -> EndpointState) -> (PortId -> EndpointState) -> Element
  -> {size : Pos, hs : List (H.Html PortEvent), ins : Dict PortId Pos, outs : Dict PortId Pos}
viewElement inState outState e =
  let
    midSpaceWidth = 5
    conWidth = 30
    headHeight = 25
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
    input idx pid =
      let
        (x, y) = inputLoc idx
        commonAttrs = [SA.cx <| toString x, SA.cy <| toString y, SA.r "10"]
        stateAttrs = case inState pid of
            EsInactive -> [SA.fill "grey"]
            EsNormal -> [SA.fill "blue", SE.onMouseDown <| PeDragStart (DragFromInput pid) (x, y)]
            EsAdd -> SA.fill "green" :: dragTargetActions pid
            EsRemove -> SA.fill "red" :: dragTargetActions pid
        inputPath = SA.d <| String.join " "
          [ "M0 " ++ toString (y - 10)
          , "a 10 10 0 0 0 0 20"
          , "h" ++ toString conWidth
          , "v-20"
          , "Z"
          ]
      in S.path (inputPath :: commonAttrs ++ stateAttrs) []
    outputLoc idx = (totalWidth, firstConCenter + (conHeight * idx))
    output idx pid =
      let
        (x, y) = outputLoc idx
        commonAttrs = [SA.cx <| toString x, SA.cy <| toString y, SA.r "10"]
        stateAttrs = case outState pid of
            EsInactive -> [SA.fill "grey"]
            EsNormal -> [SA.fill "blue", SE.onMouseDown <| PeDragStart (DragFromOutput pid) (x, y)]
            EsAdd -> SA.fill "green" :: dragTargetActions pid
            EsRemove -> SA.fill "red" :: dragTargetActions pid
        outputPath = SA.d <| String.join " "
          [ "M" ++ toString (totalWidth - conWidth) ++ " " ++ toString (y + 10)
          , "v-20"
          , "h" ++ toString conWidth
          , "a 10 10 0 0 1 0 20"
          , "Z"
          ]
      in S.path (outputPath :: commonAttrs ++ stateAttrs) []
    box = S.rect
      [ SA.width <| toString totalWidth, SA.height <| toString totalHeight, SA.fill "lightgrey", SA.stroke "black" ]
      []
    heading = S.foreignObject
      [ SA.transform <| "translate" ++ toString (headBorder/2,headBorder/2)
      , SA.width <| toString <| totalWidth - headBorder, SA.height <| toString <| headHeight - headBorder]
      [ H.body [HA.attribute "xmlns" "http://www.w3.org/1999/xhtml"] [H.text <| .desc e] ]
    inHs = List.indexedMap input <| Dict.keys <| .inputs e
    outHs = List.indexedMap output <| Dict.keys <| .outputs e
    insPos = Dict.fromList <| List.indexedMap (\idx k -> (k, inputLoc idx)) <| Dict.keys <| .inputs e
    outsPos = Dict.fromList <| List.indexedMap (\idx k -> (k, outputLoc idx)) <| Dict.keys <| .outputs e
  in {size = (totalWidth, totalHeight), hs = box :: heading :: inHs ++ outHs, ins = insPos, outs = outsPos}

viewElements : (TermId -> EndpointState) -> (TermId -> EndpointState) -> Dict ElemId Element -> {hs : List (H.Html FlowEvent), ins : Dict TermId Pos, outs : Dict TermId Pos}
viewElements inState outState elems =
  let
    elemViews = Dict.map (\eid e -> viewElement (\pid -> inState (eid, pid)) (\pid -> outState (eid, pid)) e) elems
    positionedElemViews = Dict.fromList <| List.indexedMap (\idx (eid, ev) -> (eid, ((idx * 100, idx * 50), ev))) <| Dict.toList elemViews
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
        endsAt itid = (opos, posOf <| DragFromInput itid)
      in List.map endsAt <| Set.toList <| .cons o
    conLines (eid, e) = List.concatMap (oConLines eid) <| Dict.toList <| .outputs e
    lineEnds = List.concatMap conLines <| Dict.toList elems
    lines = List.map (\((ax, ay), (bx, by)) -> S.line [SA.x1 <| toString ax, SA.x2 <| toString bx, SA.y1 <| toString ay, SA.y2 <| toString by, SA.stroke "black", SA.strokeWidth "2"] []) lineEnds
  in {hs = lines ++ hGroups, ins = insPos, outs = outsPos}

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
            dragLineStyle = case end of
                Nothing -> [SA.strokeWidth "1", SA.strokeDasharray "10,10", SA.stroke "blue"]
                Just endK -> if Set.member endK possibleNew
                    then [SA.strokeWidth "2", SA.strokeDasharray "5,5", SA.stroke "green"]
                    else if Set.member endK existing
                        then [SA.strokeWidth "2", SA.strokeDasharray "5,5", SA.stroke "red"]
                        else [SA.strokeWidth "1", SA.strokeDasharray "10,10", SA.stroke "grey"]
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
            , mDrag = Just asDragLine
            , globalAttrs = [mouseMove Dragging, SE.onMouseUp StopDrag, preventDragging, HE.onMouseLeave StopDrag]
            }
    {hs, ins, outs} = viewElements inputState outputState <| .elements m
    mDragLine = Maybe.andThen (\f -> f ins outs) mDrag
  in S.svg globalAttrs <| maybeToList mDragLine ++ hs

setToggleElem : comparable -> Set comparable -> Set comparable
setToggleElem elem elems = if Set.member elem elems
    then Set.remove elem elems
    else Set.insert elem elems

updateFlow : FlowEvent -> FlowModel -> FlowModel
updateFlow e m = case e of
    StartDrag dse pos -> {m | dragging = Just {pos = pos, start = dse, end = Nothing}}
    StopDrag ->
      let
        newElems = case .dragging m of
            Nothing -> .elements m
            Just {start, end} -> case end of
                Nothing -> .elements m
                Just endK -> case start of
                    DragFromInput startK -> toggleCon endK startK
                    DragFromOutput startK -> toggleCon startK endK
        toggleCon (outEid, outPid) inTid =
          let
            tc2 = Maybe.map <| \c -> {c | cons = setToggleElem inTid <| .cons c}
            tc = Maybe.map <| \e -> {e | outputs = Dict.update outPid tc2 <| .outputs e}
          in Dict.update outEid tc <| .elements m
      in {m | dragging = Nothing, elements = newElems}
    DragSetEnd mk -> case .dragging m of
        Nothing -> m
        Just d -> {m | dragging = Just {d | end = mk}}
    Dragging x y -> case .dragging m of
        Nothing -> m
        Just d -> {m | dragging = Just {d | pos = (x, y)}}
    DoSodAll -> m
