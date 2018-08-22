module ArrayView exposing (viewArray, defaultChildChoice, chosenChildSegs, remoteChildSegs, rectifyEdits, arrayActionStateUpdate)

import Html as H exposing (Html)
import Html.Events as HE
import Html.Attributes as HA
import Dict exposing (Dict)
import Set exposing (Set)
import Json.Encode as JE
import Tuple exposing (second)

import Futility exposing (dictMapMaybe, Either(..))
import SequenceOps exposing (SeqOp(..), applySeqOps, inject)
import ClTypes exposing (Path, Seg, ArrayDefinition, Namespace, Editable(..), PostDefinition)
import ClNodes exposing (Node(ContainerNode), ContainerNodeT)
import Form exposing (FormState(..))
import EditTypes exposing (NodeEdit(NeChildren), EditEvent(..), NaChildrenT(..), PaChildrenT, NeChildrenT, emptyPartial, NeConstT)
import RemoteState exposing (Valuespace, Postability(..))
import Digests exposing (Cops)
import DragAndDrop exposing (dragStartAttr, dragOverAttr, onDragStart, onDragEnter, onDrop, EffectAllowed(EaMove))
import TupleViews exposing (viewConstTupleEdit, asSubmittable)

defaultChildChoice : Maybe (List Seg) -> Set Seg
defaultChildChoice mSegs = case mSegs of
    Just (seg :: _) -> Set.singleton seg
    _ -> Set.empty

chosenChildSegs : FormState NodeEdit -> Maybe (Set Seg)
chosenChildSegs nfs = case nfs of
    FsEditing (NeChildren v) -> Just <| .chosen v
    _ -> Nothing

remoteChildSegs : Valuespace -> Path -> Maybe (List Seg)
remoteChildSegs vs p = case Dict.get p <| .nodes vs of
    Just (ContainerNode segs) -> Just <| List.map .seg segs
    _ -> Nothing

partitionRemoveOps : Dict comparable (SeqOp a) -> (Dict comparable (SeqOp a), Dict comparable (SeqOp a))
partitionRemoveOps =
  let
    isAbsent _ s = case s of
        SoAbsent -> True
        _ -> False
  in Dict.partition isAbsent

rectifyEdits : List Seg -> Dict Seg (SeqOp Seg) -> NeChildrenT -> NeChildrenT
rectifyEdits initialList cops edits =
  let
    rectifyEdit seg op es =
      let
        newDrag = case .dragging es of
            Nothing -> Nothing
            Just (startSeg, mEndSeg) -> if startSeg == seg
                then Nothing
                else if mEndSeg == Just seg
                    then Nothing
                    else .dragging es
      in case op of
        SoAbsent ->
            { es
            | chosen = Set.remove seg <| .chosen es
            , dragging = newDrag
            }
        SoPresentAfter _ -> { es | dragging = newDrag}
  in Dict.foldl rectifyEdit edits cops

arrayActionStateUpdate : NaChildrenT -> NodeEdit -> NodeEdit
arrayActionStateUpdate na ne = case na of
    NacCreate _ _ -> case ne of
        NeChildren nec -> NeChildren {nec | create = Nothing}
        _ -> ne
    _ -> ne

acMime : String
acMime = "elmweb/arrayChild"

acDragStartAttr = dragStartAttr EaMove <| Dict.singleton acMime <| JE.string "unelmable"

acDragOverAttr =
  let
    acHandler dragOverEvt = case Dict.get acMime <| .dataTransfer dragOverEvt of
        Nothing -> {preventDefault = True, stopPropagation = False}
        Just _ -> {preventDefault = True, stopPropagation = False}
  in dragOverAttr acHandler

emptyPostDefPartial : PostDefinition -> NeConstT
emptyPostDefPartial = emptyPartial << List.map second << .fieldDescs

viewArray
   : Editable -> ArrayDefinition -> Postability -> List Cops
  -> ContainerNodeT -> FormState NeChildrenT -> Maybe PaChildrenT
  -> Html (EditEvent NeChildrenT NaChildrenT)
viewArray editable arrayDef postability recentCops n s mp =
  let
    baseSegs = List.map .seg n
    recentAttrib = List.foldl (Dict.union << Dict.map (always Tuple.first)) Dict.empty recentCops
    (recentRemoves, recentMoves) = partitionRemoveOps <|
        List.foldl (Dict.union << Dict.map (always Tuple.second)) Dict.empty recentCops
    rSegs = Maybe.withDefault baseSegs <| Result.toMaybe <| applySeqOps recentMoves baseSegs
    editState = case s of
        FsViewing -> {chosen = defaultChildChoice <| Just rSegs, create = Nothing, dragging = Nothing}
        FsEditing v -> v
    chosenChange op = EeUpdate {editState | chosen = op <| .chosen editState}
    content = case .create editState of
        Just partialCreate ->
          let
            atomDefs = case postability of
                PostableLoaded postDef -> List.map Tuple.second <| .fieldDescs postDef
                _ -> []
            partialVals = .vals partialCreate
            editForm = H.map (\pcs -> EeUpdate <| {editState | create = Just {partialCreate | vals = pcs}}) <|
                viewConstTupleEdit atomDefs partialVals
          in case asSubmittable atomDefs partialVals of
            Just wvs -> [editForm, H.button [HE.onClick <| EeSubmit <| NacCreate (Maybe.map Left <| .ref partialCreate) wvs] [H.text "Create"]]
            Nothing -> [editForm]
        Nothing -> case editable of
            Editable ->
              let
                (pendingRemoves, pendingMoves) = partitionRemoveOps <| Maybe.withDefault Dict.empty <| Maybe.map .childMods mp
                allSegs = Maybe.withDefault rSegs <| Result.toMaybe <| applySeqOps pendingMoves rSegs
                addBtn mPrevSeg =
                  let
                    attrs = case postability of
                        PostableLoaded postDef ->
                          [ HE.onClick <| EeUpdate
                            { editState
                            | create = Just {ref = mPrevSeg, vals = emptyPostDefPartial postDef}
                            }
                          , HA.style [("background", "green")]
                          ]
                        _ ->
                          [ HA.style [("background", "grey")] ]
                  in H.span attrs [H.text "+"]
                delBtn seg = H.span
                  [ HE.onClick <| EeSubmit <| NacDelete seg
                  , HA.style [("background", "red")]
                  ]
                  [H.text "-"]
                (dragAttrsFor, mEndSeg) = case .dragging editState of
                    Nothing ->
                      ( \seg ->
                          [ onDragStart <| EeUpdate <| {editState | dragging = Just (seg, Nothing)}
                          , HA.draggable "true"
                          , acDragStartAttr
                          ]
                      , Nothing
                      )
                    Just (startSeg, mEndSeg) ->
                      ( \seg ->
                          let
                            dragEntered = onDragEnter <| EeUpdate <| {editState | dragging = Just (startSeg, Just seg)}
                          in if seg == startSeg
                            then [acDragOverAttr, dragEntered]
                            else [acDragOverAttr, dragEntered, onDrop <| EeSubmit <| NacMove startSeg <| Just seg]
                      , mEndSeg
                      )
                viewSeg seg =
                  let
                    removed = Dict.member seg pendingRemoves || Dict.member seg recentRemoves
                    edited = Dict.member seg pendingMoves
                    added = not <| List.member seg baseSegs
                    segText = H.text <| case (Dict.get seg recentAttrib, edited) of
                        (Just (Just att), False) -> seg ++ " (" ++ att ++ ")"
                        _ -> seg
                    segWidget = if Set.member seg <| .chosen editState
                        then H.b
                            ((HE.onClick <| chosenChange <| Set.remove seg) :: dragAttrsFor seg)
                            [segText]
                        else H.span
                            ((HE.onClick <| chosenChange <| Set.insert seg) :: dragAttrsFor seg)
                            [segText]
                  in if removed
                    then H.div [] [H.del [] [segWidget]]
                    else if added
                        then H.div [] [H.ins [] [segWidget], addBtn <| Just seg, delBtn seg]
                        else H.div [] [segWidget, addBtn <| Just seg, delBtn seg]
                segViews = List.map viewSeg allSegs
              in addBtn Nothing :: segViews
            ReadOnly ->
              let
                viewSeg seg =
                  let
                    {attrs, segStr} =
                      let
                        segText = H.text <| case Dict.get seg recentAttrib of
                            Just (Just att) -> seg ++ " (" ++ att ++ ")"
                            _ -> seg
                      in if Set.member seg <| .chosen editState
                        then
                          { segStr = H.b [] [segText]
                          , attrs = [HE.onClick <| chosenChange <| Set.remove seg]
                          }
                        else
                          { segStr = segText
                          , attrs = [HE.onClick <| chosenChange <| Set.insert seg]
                          }
                    segStyled = if Dict.member seg recentRemoves
                        then H.del [] [segStr]
                        else if Dict.member seg recentMoves
                            then H.ins [] [segStr]
                            else segStr
                  in H.div attrs [segStyled]
              in List.map viewSeg rSegs
  in H.div [] content
