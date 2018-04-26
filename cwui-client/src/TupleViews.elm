module TupleViews exposing (viewWithRecent, timeViewer, timeEditor)

import Html exposing (..)
import Html.Attributes as HA exposing (..)
import Html.Events exposing (onInput, onClick)

import Futility exposing (itemAtIndex, castMaybe, castList, replaceIdx, Either(..), maybeToList, zip, allGood)
import ClTypes exposing (Bounds, Attributee, TypeName, WireValue, asWord8, asFloat, asDouble, asString, asTime, AtomDef(..), TupleDefinition, Time)
import EditTypes exposing (EditEvent(..), NeConstT, NaConstT, pEnumConv, pTimeConv, pStringConv, pFloatConv, PartialEdit(..), PartialTime, asFull, emptyPartial, fullPartial)
import Form exposing (AtomState(..), castAs, FormState(..))
import ClNodes exposing (ConstDataNodeT)
import Digests exposing (ConstChangeT)
import Limits exposing (maxWord64, maxWord32)

type EditTarget k v f a
  = Editable k (Maybe v) (FormState f) (Maybe a)
  | ReadOnly v

viewWithRecent
   : Bool -> TupleDefinition -> List ConstChangeT
  -> Maybe ConstDataNodeT -> FormState NeConstT -> Maybe NaConstT
  -> Html (EditEvent NeConstT NaConstT)
viewWithRecent editable def recent mn fs mp =
  let
    (mSub, v) = viewWithRecentNoSubmission editable def recent mn fs mp
    vUp = Html.map EeUpdate v
  in case mSub of
    Just sub -> if Just sub == currentRemote (Maybe.map (Tuple.second << .values) mn) mp
        then vUp
        else div [] [vUp, button [onClick <| EeSubmit sub] [text "Apply"]]
    Nothing -> vUp

viewWithRecentNoSubmission
   : Bool -> TupleDefinition -> List ConstChangeT
  -> Maybe ConstDataNodeT -> FormState NeConstT -> Maybe NaConstT
  -> (Maybe NaConstT, Html NeConstT)
viewWithRecentNoSubmission editable def recent mn fs mp =
  let
    recentAttrVals = List.map (\(ma, _, wvs) -> (ma, wvs)) recent
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
    (mSubs, compV) = constDataComp def comps
    mSub = case mSubs of
        [((), ms)] -> ms
        _ -> Nothing
  in (mSub, Html.map cleanResult compV)

constDataComp
   : TupleDefinition
  -> List (Html a, EditTarget k (List WireValue) NeConstT NaConstT)
  -> (List (k, Maybe NaConstT), Html (Either a (k, NeConstT)))
constDataComp def values =
  let
    ads = List.map Tuple.second <| .types def
    viewRow (sourceInfo, et) (kSubs, cells) =
      let
        (newKSubs, tupV) = case et of
            ReadOnly wvs -> (kSubs, viewConstTuple ads wvs)
            Editable k mwvs fs mp ->
              let
                (mSub, tupV) = viewConstTupleEdit ads mwvs fs mp
              in ((k, mSub) :: kSubs, Html.map (\e -> (k, e)) <| tupV)
      in (newKSubs, cells ++ [div [] [Html.map Left sourceInfo], div [] [Html.map Right tupV]])
    (kSubs, cells) = List.foldl viewRow ([], []) values
  in (kSubs, div [style [("display", "grid"), ("grid-template-columns", "auto auto")]] cells)

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
    ADDouble bounds -> castedView asDouble <| floatViewer bounds
    ADString (reString, _) -> castedView asString <| textViewer reString
    ADRef ty -> castedView asString <| refViewer ty
    _ -> text <| "View not implemented: " ++ toString def

textViewer : String -> String -> Html a
textViewer reString s = text s

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

currentRemote : Maybe a -> Maybe a -> Maybe a
currentRemote mv mp = case mp of
    Just p -> Just p
    Nothing -> mv

-- FIXME: What if the defs change during editing?
getPartials
  : List AtomDef -> Maybe (List WireValue) -> FormState NeConstT
  -> List PartialEdit
getPartials defs mv s = case s of
    FsViewing -> case mv of
        Nothing -> emptyPartial defs
        Just v -> fullPartial defs v
    FsEditing pvs -> pvs

asSubmittable : List AtomDef -> List PartialEdit -> Maybe (List WireValue)
asSubmittable defs pvs = allGood asFull (zip pvs defs)

viewAtomEditors : List AtomDef -> List PartialEdit -> FormState NeConstT -> List (Html NeConstT)
viewAtomEditors defs mainPvs s =
  let
    indexUpdate pvs idx pv = Result.withDefault pvs <| replaceIdx idx pv pvs
    vae pvs i d s = Html.map (indexUpdate pvs i) <| viewAtomEdit d s
    aevs pvs aess = List.map3 (vae pvs) (List.range 0 <| List.length defs) defs aess
  in aevs mainPvs <| List.map AsEditing mainPvs

viewConstTupleEdit
   : List AtomDef -> Maybe (List WireValue) -> FormState NeConstT -> Maybe (List WireValue)
   -> (Maybe (List WireValue), Html NeConstT)
viewConstTupleEdit defs mv s mp =
  let
    pvs = getPartials defs mv s
    tae = viewAtomEditors defs pvs s
  in (asSubmittable defs pvs, span [] tae)

viewAtomEdit : AtomDef -> AtomState WireValue PartialEdit -> Html PartialEdit
viewAtomEdit d =
  let
    castedView toA peConv h swv = Html.map (.wrap peConv) <| case castAs toA (.unwrap peConv) swv of
        Err msg -> text msg
        Ok sa -> h sa
  in case d of
    ADEnum opts -> castedView asWord8 pEnumConv <| enumEditor opts
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
