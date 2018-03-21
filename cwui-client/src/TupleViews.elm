module TupleViews exposing (viewWithRecent, viewConstNodeEdit)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

import Futility exposing (itemAtIndex, castMaybe, castList, replaceIdx)
import ClTypes exposing (Bounds, Attributee, TypeName, WireValue(..), asWord8, asFloat, asString, AtomDef(..), TupleDefinition, Liberty(..))
import EditTypes exposing (EditEvent(..), NeConstT, NaConstT)
import Form exposing (AtomEditState(..), castAes, FormState(..))
import ClNodes exposing (ConstDataNodeT)
import Digests exposing (DataChange(ConstChange))

viewWithRecent : TupleDefinition -> Maybe ConstDataNodeT -> List DataChange -> Html a
viewWithRecent def mn recent =
  let
    mCd dc = case dc of
        ConstChange ma _ wvs -> Just (ma, wvs)
        _ -> Nothing
    recentAttrVals = List.filterMap mCd recent
    attrVals = case mn of
        Nothing -> recentAttrVals
        Just n -> .values n :: recentAttrVals
    finalVal = List.length attrVals - 1
    sourceInfo idx ma = text <| toString (idx - finalVal) ++ Maybe.withDefault "" ma
    asComp idx (ma, wvs) = (sourceInfo idx ma, Cannot, wvs)
    comps = List.indexedMap asComp attrVals
  in constDataComp def comps

viewConstTuple : TupleDefinition -> Liberty -> List WireValue -> Html a
viewConstTuple td lib wvs =
  let
    atomTypes = List.map Tuple.second <| .types td
  in span [] <| List.map2 (viewAtom lib) atomTypes wvs

constDataComp : TupleDefinition -> List (Html a, Liberty, List WireValue) -> Html a
constDataComp def values =
  let
    viewRow (sourceInfo, lib, wvs) = [div [] [sourceInfo], div [] [viewConstTuple def lib wvs]]
    cells = List.concatMap viewRow values
  in div [style [("display", "grid"), ("grid-template-columns", "auto auto")]] cells

viewAtom : Liberty -> AtomDef -> WireValue -> Html a
viewAtom lib def wv =
  -- FIXME: Ignores liberty
  let
    castedView : (WireValue -> Result String b) -> (b -> Html a) -> Html a
    castedView c h = case c wv of
        Ok a -> h a
        Err msg -> text msg
  in case def of
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

viewConstNodeEdit
   : TupleDefinition -> Maybe ConstDataNodeT -> FormState NeConstT -> Maybe NaConstT
  -> Html (EditEvent NeConstT NaConstT)
viewConstNodeEdit d mn s mp = viewConstTupleEdit (List.map Tuple.second <| .types d) (Maybe.map (Tuple.second << .values) mn) s mp

viewConstTupleEdit
   : List AtomDef -> Maybe (List WireValue) -> FormState NeConstT -> Maybe (List WireValue)
   -> Html (EditEvent NeConstT NaConstT)
viewConstTupleEdit defs mv s mp =
  let
    nDefs = List.length defs
    current = case mv of
        Nothing -> List.repeat nDefs Nothing
        Just v -> List.map Just v
    toAes mev = case mev of
        Nothing -> AesUnfilled
        Just ev -> AesEditing ev
    (editBase, atomEditStates) = case s of
        FsViewing -> (current, List.repeat nDefs AesViewing)
        FsEditing mevs -> (mevs, List.map toAes mevs)
    asPartial idx wv = EeUpdate <| Result.withDefault editBase <| replaceIdx idx (Just wv) editBase
    atomEditor idx def mwv aes = Html.map (asPartial idx) <| viewAtomEdit def mwv aes
    atomEditors = List.map4 atomEditor (List.range 0 nDefs) defs current atomEditStates
    filledFields = List.filterMap identity editBase
    allFilled = List.length filledFields == List.length defs
    asPending = Just filledFields == mp
    content = if allFilled && not asPending
      then button [onClick <| EeSubmit filledFields] [text "Apply"] :: atomEditors
      else atomEditors
  in span [] content

viewAtomEdit : AtomDef -> Maybe WireValue -> AtomEditState WireValue -> Html WireValue
viewAtomEdit d =
  let
    castedView
       : (WireValue -> Result String a) -> (a -> WireValue) -> (Maybe a -> AtomEditState a -> Html a)
       -> Maybe WireValue -> AtomEditState WireValue -> Html WireValue
    castedView toA toWv h mwv swv = Html.map toWv <| case castMaybe toA mwv of
        Err msg -> text msg
        Ok ma -> case castAes toA swv of
            Err msg -> text msg
            Ok sa -> h ma sa
  in case d of
    ADEnum opts -> castedView asWord8 WvWord8 <| enumEditor opts
    _ -> \_ _ -> text <| "Implement me: " ++ toString d

enumEditor : List String -> Maybe Int -> AtomEditState Int -> Html Int
enumEditor opts me se = text "shuffle"
-- select
--     [onInput (Result.withDefault -1 << String.toInt)]
--     (List.indexedMap (\i o -> option [value (toString i), selected (i == e)] [text o]) opts)
