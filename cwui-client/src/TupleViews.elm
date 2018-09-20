module TupleViews exposing
  ( viewWithRecent, viewWithRecentNoSubmission, timeViewer, timeEditor, pInfo
  , viewConstTupleEdit, asSubmittable)

import Html exposing (..)
import Html.Attributes as HA exposing (..)
import Html.Events exposing (onInput, onClick)

import Futility exposing (itemAtIndex, castMaybe, castList, replaceIdx, Either(..), maybeToList, zip, allGood, lastJust, last)
import ClTypes exposing (Bounds, Attributee, Seg, WireValue, asFloat, asDouble, asString, asTime, AtomDef(..), TupleDefinition, Time, Editable(..), Definition, asWord32)
import EditTypes exposing (EditEvent(..), NeConstT, NaConstT, pEnumConv, pTimeConv, pStringConv, pFloatConv, PartialEdit(..), PartialTime, asFull, emptyPartial, fullPartial)
import Form exposing (AtomState(..), castAs, FormState(..))
import ClNodes exposing (ConstDataNodeT)
import Digests exposing (ConstChangeT)
import Limits exposing (maxWord64, maxWord32)
import Tagged.Tagged exposing (Tagged)

type EditTarget k v p
  = EditableTarget k p
  | ReadOnlyTarget v

viewWithRecent
   : Editable -> TupleDefinition -> List ConstChangeT
  -> ConstDataNodeT -> FormState NeConstT -> Maybe NaConstT
  -> Html (EditEvent NeConstT NaConstT)
viewWithRecent editable def recent n fs mp =
  let
    ads = List.map Tuple.second <| .types def
    (latestPartial, mLatest, mSub) = pInfo ads n recent fs mp
    v = viewWithRecentNoSubmission editable ads recent n latestPartial
    vUp = Html.map EeUpdate v
  in case mSub of
    Just sub -> if Just sub == mLatest
        then vUp
        else div [] [vUp, button [onClick <| EeSubmit sub] [text "Apply"]]
    Nothing -> vUp

pInfo
   : List AtomDef -> ConstDataNodeT -> List ConstChangeT
   -> FormState NeConstT -> Maybe NaConstT
   -> (NeConstT, Maybe NaConstT, Maybe NaConstT)
pInfo ads n recent fs mp =
  let
    attrVals = upstreamStates n recent
    mRemote = Maybe.map Tuple.second <| last attrVals
    partial = getPartials ads mRemote fs mp
  in (partial, lastJust mRemote mp, asSubmittable ads partial)

upstreamStates : ConstDataNodeT -> List ConstChangeT -> List (Maybe Attributee, List WireValue)
upstreamStates n recent =
    .values n :: List.map (\(ma, _, wvs) -> (ma, wvs)) recent

viewWithRecentNoSubmission
   : Editable -> List AtomDef -> List ConstChangeT
  -> ConstDataNodeT -> NeConstT
  -> Html NeConstT
viewWithRecentNoSubmission editable ads recent n latestPartial =
  let
    attrVals = upstreamStates n recent
    finalVal = List.length attrVals - 1
    sourceInfo idx ma = text <| toString (idx - finalVal) ++ Maybe.withDefault "" ma
    latestControls = case editable of
      Editable -> text "Staging controls?"
      ReadOnly -> text "Latest"
    asComp idx (ma, wvs) =
      let
        isLatest = idx == finalVal
        si = if isLatest
          then latestControls
          else sourceInfo idx ma
        et = if isLatest && editable == Editable
          then EditableTarget () latestPartial
          else ReadOnlyTarget wvs
      in (si, et)
    valComps = List.indexedMap asComp attrVals
    comps = case (editable, valComps) of
        (Editable, []) -> [(latestControls, EditableTarget () latestPartial)]
        _ -> valComps
    cleanResult r = case r of
        Left a -> never a
        Right (_, evt) -> evt
    compV = constDataComp ads comps
  in Html.map cleanResult compV

constDataComp
   : List AtomDef
  -> List (Html a, EditTarget k (List WireValue) NeConstT)
  -> Html (Either a (k, NeConstT))
constDataComp ads values =
  let
    viewRow (sourceInfo, et) =
      let
        tupV = case et of
            ReadOnlyTarget wvs -> viewConstTuple ads wvs
            EditableTarget k p -> Html.map ((,) k) <| viewConstTupleEdit ads p
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
    ADEnum opts -> castedView asWord32 <| enumViewer opts
    ADFloat bounds -> castedView asFloat <| floatViewer bounds
    ADDouble bounds -> castedView asDouble <| floatViewer bounds
    ADString (reString, _) -> castedView asString <| textViewer reString
    ADRef ty -> castedView asString <| refViewer ty
    _ -> text <| "View not implemented: " ++ toString def

textViewer : String -> String -> Html a
textViewer reString s = text s

refViewer : Tagged Definition Seg -> String -> Html a
refViewer tn tgt = text tgt

floatViewer : Bounds Float -> Float -> Html a
floatViewer b f = text <| toString f

enumViewer : List String -> Int -> Html a
enumViewer opts idx = text <| case itemAtIndex idx opts of
        Just v -> v
        Nothing -> "Enum index out of range"

timeViewer : Bounds Time -> Time -> Html a
timeViewer _ (s, f) = text <| toString s ++ ":" ++ toString f

-- FIXME: What if the defs change during editing?
getPartials
  : List AtomDef -> Maybe (List WireValue) -> FormState NeConstT
  -> Maybe (List WireValue) -> List PartialEdit
getPartials defs mv s mp = case s of
    FsViewing -> case lastJust mv mp of
        Nothing -> emptyPartial defs
        Just v -> fullPartial defs v
    FsEditing pvs -> pvs

asSubmittable : List AtomDef -> List PartialEdit -> Maybe (List WireValue)
asSubmittable defs pvs = allGood asFull (zip pvs defs)

viewConstTupleEdit : List AtomDef -> NeConstT -> Html NeConstT
viewConstTupleEdit defs mainPvs =
  let
    indexUpdate pvs idx pv = Result.withDefault pvs <| replaceIdx idx pv pvs
    vae pvs i d s = Html.map (indexUpdate pvs i) <| viewAtomEdit d s
    aevs pvs aess = List.map3 (vae pvs) (List.range 0 <| List.length defs) defs aess
  in span [] <| aevs mainPvs <| List.map AsEditing mainPvs

viewAtomEdit : AtomDef -> AtomState WireValue PartialEdit -> Html PartialEdit
viewAtomEdit d =
  let
    castedView toA peConv h swv = Html.map (.wrap peConv) <| case castAs toA (.unwrap peConv) swv of
        Err msg -> text msg
        Ok sa -> h sa
  in case d of
    ADEnum opts -> castedView asWord32 pEnumConv <| enumEditor opts
    ADTime bounds -> castedView asTime pTimeConv <| timeEditor bounds
    ADString (reString, _) -> castedView asString pStringConv <| textEditor reString
    ADFloat bounds -> castedView asFloat pFloatConv <| floatEditor bounds
    ADDouble bounds -> castedView asDouble pFloatConv <| floatEditor bounds
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

textEditor : String -> AtomState String String -> Html String
textEditor reString aes = case aes of
    AsViewing upstream ev -> span [onClick ev] [textViewer reString upstream]
    AsEditing ev -> input [attribute "regex" reString, value ev, onInput identity] []

floatEditor : Bounds Float -> AtomState Float (Maybe Float) -> Html (Maybe Float)
floatEditor bounds aes = case aes of
    AsViewing upstream ev -> span [onClick ev] [floatViewer bounds upstream]
    AsEditing ev ->
      let
        attrs =
          [ type_ "number", onInput <| Result.toMaybe << String.toFloat ]
          ++ maybeToList (Maybe.map (HA.min << toString) <| .minBound bounds)
          ++ maybeToList (Maybe.map (HA.max << toString) <| .maxBound bounds)
          ++ maybeToList (Maybe.map (value << toString) ev)
      in input attrs []
