module TupleViews exposing (viewConstTuple, viewConstNodeEdit)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

import Futility exposing (itemAtIndex, castMaybe, castList, replaceIdx)
import ClTypes exposing (Bounds, Attributee, TypeName, WireValue(..), asWord8, asFloat, asString, AtomDef(..), TupleDefinition)
import EditTypes exposing (EditEvent(..), NeConstT, NaConstT)
import Form exposing (AtomEditState(..), castAes, FormState(..))
import ClNodes exposing (ConstDataNodeT)
import Digests exposing (DataChange(ConstChange))

viewConstTuple : TupleDefinition -> Maybe ConstDataNodeT -> List DataChange -> Html a
viewConstTuple td mn recent =
  let
    mCd dc = case dc of
        ConstChange ma _ wvs -> Just (ma, wvs)
        _ -> Nothing
    recentAttrVals = List.filterMap mCd recent
    attrVals = case mn of
        Nothing -> recentAttrVals
        Just n -> .values n :: recentAttrVals
    atomTypes = List.map Tuple.second <| .types td
    emptyAtomVals = List.repeat (List.length atomTypes) []
    consAttrVals (ma, wvs) accs = List.map2 (\wv acc -> (ma, wv) :: acc) wvs accs
    atomVals = List.foldr consAttrVals emptyAtomVals attrVals
  in span [] <| List.map2 viewAtom atomTypes atomVals

takeLatest : (Maybe Attributee -> a -> Html b)  -> List (Maybe Attributee, a) -> Html b
takeLatest h l = case l of
    ((ma, v) :: []) -> h ma v
    (_ :: remainder) -> takeLatest h remainder
    [] -> text "No latest value"

viewAtom : AtomDef -> List (Maybe Attributee, WireValue) -> Html a
viewAtom def avs =
  let
    castAttributed c (ma, a) = Result.map (\b -> (ma, b)) <| c a
    castedView : (WireValue -> Result String b) -> (List (Maybe Attributee, b) -> Html a) -> Html a
    castedView c h = case castList (castAttributed c) avs of
        Ok vs -> h vs
        Err msg -> text msg
  in case def of
    ADEnum opts -> castedView asWord8 <| enumViewer opts
    ADFloat bounds -> castedView asFloat <| takeLatest <| floatViewer bounds
    ADString re -> castedView asString <| takeLatest <| textViewer re
    ADRef ty -> castedView asString <| takeLatest <| refViewer ty
    _ -> text <| "View not implemented: " ++ toString def

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

textViewer : String -> Maybe Attributee -> String -> Html a
textViewer re ma s = text s

refViewer : TypeName -> Maybe Attributee -> String -> Html a
refViewer tn ma tgt = text tgt

floatViewer : Bounds Float -> Maybe Attributee -> Float -> Html a
floatViewer b ma f = text <| toString f

enumViewer : List String -> List (Maybe Attributee, Int) -> Html a
enumViewer opts vs =
  let
    optString idx = case itemAtIndex idx opts of
        Just v -> v
        Nothing -> "Enum index out of range"
    attrString ma = Maybe.withDefault "" <| Maybe.map (\a -> a ++ ": ") ma
    combinedStr (ma, idx) = attrString ma ++ optString idx
  in text <| String.join " -> " <| List.map combinedStr vs

enumEditor : List String -> Maybe Int -> AtomEditState Int -> Html Int
enumEditor opts me se = text "shuffle"
-- select
--     [onInput (Result.withDefault -1 << String.toInt)]
--     (List.indexedMap (\i o -> option [value (toString i), selected (i == e)] [text o]) opts)
