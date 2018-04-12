module TupleViews exposing (viewWithRecent)

import Html exposing (..)
import Html.Attributes as HA exposing (..)
import Html.Events exposing (onInput, onClick)

import Futility exposing (itemAtIndex, castMaybe, castList, replaceIdx, Either(..), maybeToList)
import ClTypes exposing (Bounds, Attributee, TypeName, WireValue(..), asWord8, asFloat, asString, asTime, AtomDef(..), TupleDefinition, Time)
import EditTypes exposing (EditEvent(..), NeConstT, NaConstT, pEnumConv, pTimeConv, PartialEdit(..), PartialTime)
import Form exposing (AtomState(..), castAs, FormState(..))
import ClNodes exposing (ConstDataNodeT)
import Digests exposing (DataChange(ConstChange))
import Limits exposing (maxWord64, maxWord32)

type EditTarget k v f a
  = Editable k (Maybe v) (FormState f) (Maybe a)
  | ReadOnly v

viewWithRecent
   : Bool -> TupleDefinition -> List DataChange
  -> Maybe ConstDataNodeT -> FormState NeConstT -> Maybe NaConstT
  -> Html (EditEvent NeConstT NaConstT)
viewWithRecent editable def recent mn fs mp =
  let
    -- FIXME: Not otherwise dealing with stuff like this here, sort above?
    mCd dc = case dc of
        ConstChange ma _ wvs -> Just (ma, wvs)
        _ -> Nothing
    recentAttrVals = List.filterMap mCd recent
    attrVals = case mn of
        Nothing -> recentAttrVals
        Just n -> .values n :: recentAttrVals
    finalVal = List.length attrVals - 1
    sourceInfo idx ma = text <| toString (idx - finalVal) ++ Maybe.withDefault "" ma
    latestControls = if editable
      then text "Staging controls?"
      else text "Latest"
    asComp idx (ma, wvs) =
      let
        isLatest = idx == finalVal
        si = if isLatest
          then latestControls
          else sourceInfo idx ma
        et = if isLatest && editable
          then Editable () (Just wvs) fs mp
          else ReadOnly wvs
      in (si, et)
    valComps = List.indexedMap asComp attrVals
    comps = case (editable, valComps) of
        (True, []) -> [(latestControls, Editable () Nothing fs mp)]
        _ -> valComps
    cleanResult r = case r of
        Left a -> never a
        Right (_, evt) -> evt
  in Html.map cleanResult <| constDataComp def comps

constDataComp
   : TupleDefinition
  -> List (Html a, EditTarget k (List WireValue) NeConstT NaConstT)
  -> Html (Either a (k, EditEvent NeConstT NaConstT))
constDataComp def values =
  let
    ads = List.map Tuple.second <| .types def
    viewRow (sourceInfo, et) =
      let
        tupV = case et of
            ReadOnly wvs -> viewConstTuple ads wvs
            Editable k mwvs fs mp -> Html.map (\e -> (k, e)) <| viewConstTupleEdit ads mwvs fs mp
      in [div [] [Html.map Left sourceInfo], div [] [Html.map Right tupV]]
    cells = List.concatMap viewRow values
  in div [style [("display", "grid"), ("grid-template-columns", "auto auto")]] cells

viewConstTuple : List AtomDef -> List WireValue -> Html a
viewConstTuple ads wvs = span [] <| List.map2 viewAtom ads wvs

viewAtom : AtomDef -> WireValue -> Html a
viewAtom def wv =
  let
    castedView : (WireValue -> Result String b) -> (b -> Html a) -> Html a
    castedView c h = case c wv of
        Ok a -> h a
        Err msg -> text msg
  in case def of
    ADTime bounds -> castedView asTime <| timeViewer bounds
    ADEnum opts -> castedView asWord8 <| enumViewer opts
    ADFloat bounds -> castedView asFloat <| floatViewer bounds
    ADString re -> castedView asString <| textViewer re
    ADRef ty -> castedView asString <| refViewer ty
    _ -> text <| "View not implemented: " ++ toString def

textViewer : String -> String -> Html a
textViewer re s = text s

refViewer : TypeName -> String -> Html a
refViewer tn tgt = text tgt

floatViewer : Bounds Float -> Float -> Html a
floatViewer b f = text <| toString f

enumViewer : List String -> Int -> Html a
enumViewer opts idx = text <| case itemAtIndex idx opts of
        Just v -> v
        Nothing -> "Enum index out of range"

timeViewer : Bounds Time -> Time -> Html a
timeViewer _ (s, f) = text <| toString s ++ ":" ++ toString f

viewConstNodeEdit
   : TupleDefinition -> Maybe ConstDataNodeT -> FormState NeConstT -> Maybe NaConstT
  -> Html (EditEvent NeConstT NaConstT)
viewConstNodeEdit d mn s mp = viewConstTupleEdit (List.map Tuple.second <| .types d) (Maybe.map (Tuple.second << .values) mn) s mp

allGood : (a -> Maybe b) -> List a -> Maybe (List b)
allGood f l = case l of
    [] -> Just []
    (x :: xs) -> Maybe.andThen (\y -> Maybe.map (\ys -> y :: ys) <| allGood f xs) <| f x

viewConstTupleEdit
   : List AtomDef -> Maybe (List WireValue) -> FormState NeConstT -> Maybe (List WireValue)
   -> Html (EditEvent NeConstT NaConstT)
viewConstTupleEdit defs mv s mp =
  let
    nDefs = List.length defs
    indexUpdate pvs idx pv = EeUpdate <| Result.withDefault pvs <| replaceIdx idx pv pvs
    vae pvs i d s = Html.map (indexUpdate pvs i) <| viewAtomEdit d s
    -- FIXME: What if the defs change during editing?
    aevs pvs aess = List.map3 (vae pvs) (List.range 0 nDefs) defs aess
    tupView vs =
      let
        pvs = List.map2 asPartial defs <| List.map Just vs
      in aevs pvs <| List.map2 AsViewing vs pvs
    tupEdit pvs =
      let
        atomEditStates = List.map AsEditing pvs
        thing = aevs pvs atomEditStates
        currentRemote = case mp of
            Just p -> Just p
            Nothing -> mv
        content = case allGood asFull pvs of
            Just fullVals -> if Just fullVals == currentRemote
                then thing
                else button [onClick <| EeSubmit fullVals] [text "Apply"] :: thing
            Nothing -> thing
      in content
  in span [] <| case s of
    FsViewing -> case mv of
        Nothing -> tupEdit <| emptyPartial defs
        Just v -> tupView v
    FsEditing pvs -> tupEdit pvs

asFull : PartialEdit -> Maybe WireValue
asFull pe = case pe of
    PeEnum mi -> Maybe.map WvWord8 mi
    PeTime pt -> case pt of
        (Just s, Just f) -> Just <| WvTime (s, f)
        _ -> Nothing

asPartial : AtomDef -> Maybe WireValue -> PartialEdit
asPartial d mwv = case (d, mwv) of
    (ADEnum _, Just (WvWord8 w)) -> PeEnum <| Just w
    (ADEnum _, _) -> PeEnum Nothing
    (ADTime _, Just (WvTime (s, f))) -> PeTime (Just s, Just f)
    (ADTime _, _) -> PeTime (Nothing, Nothing)
    -- FIXME: This is utter tat!
    _ -> PeEnum Nothing

emptyPartial : List AtomDef -> List PartialEdit
emptyPartial = List.map (flip asPartial Nothing)

viewAtomEdit : AtomDef -> AtomState WireValue PartialEdit -> Html PartialEdit
viewAtomEdit d =
  let
    castedView toA peConv h swv = Html.map (.wrap peConv) <| case castAs toA (.unwrap peConv) swv of
        Err msg -> text msg
        Ok sa -> h sa
  in case d of
    ADEnum opts -> castedView asWord8 pEnumConv <| enumEditor opts
    ADTime bounds -> castedView asTime pTimeConv <| timeEditor bounds
    _ -> always <| text <| "Implement me: " ++ toString d

enumEditor : List String -> AtomState Int (Maybe Int) -> Html (Maybe Int)
enumEditor opts aes = case aes of
    AsViewing upstream ev -> span [onClick ev] [enumViewer opts upstream]
    AsEditing ev -> select
        [onInput <| Result.toMaybe << String.toInt]
        (List.indexedMap (\i o -> option [value <| toString i, selected <| Just i == ev] [text o]) opts)

timeEditor : Bounds Time -> AtomState Time PartialTime -> Html PartialTime
timeEditor bounds aes = case aes of
    AsViewing upstream ev -> span [onClick ev] [timeViewer bounds upstream]
    AsEditing ev ->
      let
        minB = Maybe.withDefault (0, 0) <| .minBound bounds
        maxB = Maybe.withDefault (maxWord64, maxWord32) <| .maxBound bounds
        attrsFor itemGet mItemGet replaceTgt
          = maybeToList (Maybe.map (value << toString) <| mItemGet ev)
          ++ [HA.min <| toString <| itemGet minB, HA.max <| toString <| itemGet maxB]
          ++ [type_ "number", onInput <| \s -> replaceTgt (always <| Result.toMaybe <| String.toInt s) ev]
      in span []
        [ input (attrsFor Tuple.first Tuple.first Tuple.mapFirst) []
        , input (attrsFor Tuple.second Tuple.second Tuple.mapSecond) []
        ]
