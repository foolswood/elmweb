module ArrayView exposing (viewArray, defaultChildChoice, chosenChildSegs, remoteChildSegs)

import Html as H exposing (Html)
import Html.Events as HE
import Html.Attributes as HA
import Dict exposing (Dict)
import Set exposing (Set)

import SequenceOps exposing (SeqOp(SoAbsent), applySeqOps, banish, inject)
import ClTypes exposing (Path, Seg, ArrayDefinition)
import ClNodes exposing (Node(ContainerNode), ContainerNodeT)
import Form exposing (FormState(..))
import EditTypes exposing (NodeEdit(NeChildren), EditEvent(..), NaChildrenT, NeChildrenT)
import RemoteState exposing (RemoteState)
import Digests exposing (Cops)

defaultChildChoice : Maybe (List Seg) -> Set Seg
defaultChildChoice mSegs = case mSegs of
    Just (seg :: _) -> Set.singleton seg
    _ -> Set.empty

chosenChildSegs : FormState NodeEdit -> Maybe (Set Seg)
chosenChildSegs nfs = case nfs of
    FsEditing (NeChildren v) -> Just <| .chosen v
    _ -> Nothing

remoteChildSegs : RemoteState -> Path -> Maybe (List Seg)
remoteChildSegs rs p = case Dict.get p <| .nodes rs of
    Just (ContainerNode segs) -> Just <| List.map .seg segs
    _ -> Nothing

partitionRemoveOps : Dict comparable (SeqOp a) -> (Dict comparable (SeqOp a), Dict comparable (SeqOp a))
partitionRemoveOps =
  let
    isAbsent _ s = case s of
        SoAbsent -> True
        _ -> False
  in Dict.partition isAbsent

viewArray
   : Bool -> ArrayDefinition -> List Cops
  -> Maybe ContainerNodeT -> FormState NeChildrenT -> Maybe NaChildrenT
  -> Html (EditEvent NeChildrenT NaChildrenT)
viewArray editable arrayDef recentCops mn s mp =
  let
    baseSegs = case mn of
        Just containees -> List.map .seg containees
        _ -> []
    recentAttrib = List.foldl (Dict.union << Dict.map (always Tuple.first)) Dict.empty recentCops
    (recentRemoves, recentMoves) = partitionRemoveOps <|
        List.foldl (Dict.union << Dict.map (always Tuple.second)) Dict.empty recentCops
    rSegs = Maybe.withDefault baseSegs <| Result.toMaybe <| applySeqOps recentMoves baseSegs
    editState = case s of
        FsViewing -> {chosen = defaultChildChoice <| Just rSegs, ops = Dict.empty, addSeg = ""}
        FsEditing v -> v
    chosenChange op = EeUpdate {editState | chosen = op <| .chosen editState}
    content = if editable
        then
          let
            addSeg = .addSeg editState
            -- FIXME: use the regex from the relay's definition:
            addTextEntry = H.input
              [ HA.type_ "text", HA.value addSeg
              , HE.onInput <| \s -> EeUpdate {editState | addSeg = s}
              ] []
            (pendingRemoves, pendingMoves) = partitionRemoveOps <| Maybe.withDefault Dict.empty mp
            (editRemoves, editMoves) = partitionRemoveOps <| .ops editState
            allSegs = Maybe.withDefault rSegs <| Result.toMaybe <| applySeqOps (Dict.union editMoves pendingMoves) rSegs
            canAdd = not <| List.member addSeg <| "" :: allSegs
            addBtn mPrevSeg =
              let
                attrs = if canAdd
                    then
                      [ HE.onClick <| EeUpdate
                        { ops = inject rSegs addSeg mPrevSeg <| .ops editState
                        , chosen = Set.insert addSeg <| .chosen editState
                        , addSeg = ""
                        }
                      , HA.style [("background", "green")]
                      ]
                    else
                      [ HA.style [("background", "grey")] ]
              in H.span attrs [H.text "+"]
            delBtn seg = H.span
              [ HE.onClick <| if Dict.member seg <| .ops editState
                    then EeUpdate
                      { editState
                      | ops = banish rSegs seg <| .ops editState
                      , chosen = Set.remove seg <| .chosen editState
                      }
                    else EeSubmit <| Dict.singleton seg SoAbsent
              , HA.style [("background", "red")]
              ]
              [H.text "-"]
            viewSeg seg =
              let
                removed = Dict.member seg editRemoves || Dict.member seg pendingRemoves || Dict.member seg recentRemoves
                edited = Dict.member seg editMoves || Dict.member seg pendingMoves
                added = not <| List.member seg baseSegs
                segText = H.text <| case (Dict.get seg recentAttrib, edited) of
                    (Just (Just att), False) -> seg ++ " (" ++ att ++ ")"
                    _ -> seg
                segWidget = if Set.member seg <| .chosen editState
                    then H.b [HE.onClick <| chosenChange <| Set.remove seg] [segText]
                    else H.span [HE.onClick <| chosenChange <| Set.insert seg] [segText]
              in if removed
                then H.div [] [H.del [] [segWidget]]
                else if added
                    then H.div [] [H.ins [] [segWidget], addBtn <| Just seg, delBtn seg]
                    else H.div [] [segWidget, addBtn <| Just seg, delBtn seg]
            segViews = List.map viewSeg allSegs
          in addTextEntry :: addBtn Nothing :: segViews
        else
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
