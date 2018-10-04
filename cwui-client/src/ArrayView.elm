module ArrayView exposing
  (viewArray, remoteChildSegs, rectifyEdits , arrayActionStateUpdate)

import Html as H exposing (Html)
import Html.Events as HE
import Html.Attributes as HA
import Dict exposing (Dict)
import Json.Encode as JE
import Tuple exposing (second)
import Regex exposing (Regex)

import Futility exposing (Either(..), zip, allGood)
import Tagged.Tagged exposing (Tagged(..))
import HtmlHelpers exposing (listEdit)
import SequenceOps exposing (SeqOp(..), applySeqOps)
import ClTypes exposing (Path, Seg, ArrayDefinition, Namespace, Editable(..), PostDefinition, Placeholder)
import ClNodes exposing (Node(ContainerNode), ContainerNodeT)
import Form exposing (FormState(..), AtomState(AsEditing))
import EditTypes exposing
  ( NodeEdit(NeChildren), EditEvent(..), NaChildrenT(..), PaChildrenT
  , NeChildrenT, emptyPartial, NeConstT, ChildSourceStateId)
import RemoteState exposing (Valuespace, Postability(..))
import Digests exposing (Cops)
import DragAndDrop exposing (dragStartAttr, dragOverAttr, onDragStart, onDragEnter, onDrop, EffectAllowed(EaMove))
import TupleViews exposing (viewConstTupleEdit, asSubmittable, textEditor)

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
            | dragging = newDrag
            }
        SoPresentAfter _ -> { es | dragging = newDrag}
  in Dict.foldl rectifyEdit edits cops

arrayActionStateUpdate : NaChildrenT -> NodeEdit -> NodeEdit
arrayActionStateUpdate na ne = case na of
    NacCreate _ _ _ -> case ne of
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

emptyPostDefPartial : PostDefinition -> List NeConstT
emptyPostDefPartial = List.map emptyPartial << List.map second << .fieldDescs

type alias ContainerEdit = EditEvent NeChildrenT NaChildrenT

segChooserWidget
   : ChildSourceStateId -> (Seg -> Bool) -> (Seg -> List (H.Attribute ContainerEdit))
  -> String -> Seg -> H.Html ContainerEdit
segChooserWidget cssid isSelected additionalAttrFactory segText seg =
    if isSelected seg
        then H.b
            ((HE.onClick <| EeSubmit <| NacDeselect cssid seg) :: additionalAttrFactory seg)
            [H.text segText]
        else H.span
            ((HE.onClick <| EeSubmit <| NacSelect cssid seg) :: additionalAttrFactory seg)
            [H.text segText]

viewArray
   : ChildSourceStateId -> (Seg -> Bool) -> Editable -> ArrayDefinition -> Postability -> List (Cops Seg)
  -> ContainerNodeT -> FormState NeChildrenT -> Maybe PaChildrenT
  -> Html ContainerEdit
viewArray cssid isSelected editable arrayDef postability recentCops n s mp =
  let
    baseSegs = List.map .seg n
    recentAttrib = List.foldl (Dict.union << Dict.map (always Tuple.first)) Dict.empty recentCops
    (recentRemoves, recentMoves) = partitionRemoveOps <|
        List.foldl (Dict.union << Dict.map (always Tuple.second)) Dict.empty recentCops
    rSegs = Maybe.withDefault baseSegs <| Result.toMaybe <| applySeqOps recentMoves baseSegs
    editState = case s of
        FsViewing -> {create = Nothing, dragging = Nothing}
        FsEditing v -> v
    viewSeg pendingRemoves pendingMoves additionalAttrFactory additionalElemFactory seg =
      let
        removed = Dict.member seg pendingRemoves || Dict.member seg recentRemoves
        edited = Dict.member seg pendingMoves
        added = not <| List.member seg baseSegs
        segString = case (Dict.get seg recentAttrib, edited) of
            (Just (Just att), False) -> seg ++ " (" ++ att ++ ")"
            _ -> seg
        segWidget = segChooserWidget cssid isSelected additionalAttrFactory segString seg
      in if removed
        then H.div [] [H.del [] [segWidget]]
        else if added
            then H.div [] <| H.ins [] [segWidget] :: additionalElemFactory seg
            else H.div [] <| segWidget :: additionalElemFactory seg
    content = case .create editState of
        Just partialCreate ->
          let
            atomDefs = case postability of
                PostableLoaded _ postDef -> List.map Tuple.second <| .fieldDescs postDef
                _ -> []
            partialVals = .vals partialCreate
            editForm = H.map
                (\pcs -> EeUpdate <| {editState | create = Just {partialCreate | vals = pcs}})
                <| H.div [] <| listEdit (Tuple.second) (uncurry viewConstTupleEdit) (zip atomDefs partialVals)
            phEdit = H.map
                (\ph -> EeUpdate <| {editState | create = Just {partialCreate | desiredPlaceholder = ph}})
                <| textEditor phReStr (AsEditing <| .desiredPlaceholder partialCreate)
          in case allGood (uncurry asSubmittable) (zip atomDefs partialVals) of
            Just wvs ->
              [ phEdit
              , editForm
              , H.button
                  [HE.onClick <| EeSubmit <| NacCreate
                    (asPlaceholder <| .desiredPlaceholder partialCreate)
                    (Maybe.map Right <| .ref partialCreate)
                    wvs]
                  [H.text "Create"]
              ]
            Nothing -> [phEdit, editForm]
        Nothing -> case editable of
            Editable ->
              let
                (pendingRemoves, pendingMoves) = partitionRemoveOps <| Maybe.withDefault Dict.empty <| Maybe.map .childMods mp
                allSegs = Maybe.withDefault rSegs <| Result.toMaybe <| applySeqOps pendingMoves rSegs
                addBtn mPrevSeg =
                  let
                    attrs = case postability of
                        PostableLoaded _ postDef ->
                          [ HE.onClick <| EeUpdate
                            { editState
                            | create = Just
                              { desiredPlaceholder = "ph"
                              , ref = mPrevSeg
                              , vals = emptyPostDefPartial postDef}
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
                segViews = List.map
                    (viewSeg
                        pendingRemoves pendingMoves
                        dragAttrsFor (\seg -> [addBtn <| Just seg, delBtn seg]))
                    allSegs
              in addBtn Nothing :: segViews
            -- FIXME: Should be able to post to ReadOnly arrays
            ReadOnly -> List.map (viewSeg Dict.empty Dict.empty (always []) (always [])) rSegs
  in H.div [] content

phReStr : String
phReStr = "^[a-zA-Z0-9]\\w*$"

placeholderRegex : Regex
placeholderRegex = Regex.regex phReStr

asPlaceholder : String -> Placeholder
asPlaceholder s = Tagged <| if Regex.contains placeholderRegex s
    then s else "unnamed"
