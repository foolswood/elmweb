module TupleViews exposing (viewConstTuple, viewConstNodeEdit)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

import Futility exposing (itemAtIndex, castMaybe, replaceIdx)
import ClTypes exposing (Bounds, Attributee, TypeName, WireValue(..), asWord8, asFloat, asString, AtomDef(..), TupleDefinition)
import EditTypes exposing (NodeEditEvent(..), NeConstT)
import Form exposing (AtomEditState(..), castAes, FormState(..))
import ClNodes exposing (ConstDataNodeT)

viewConstTuple : TupleDefinition -> Maybe ConstDataNodeT -> Html a
viewConstTuple td mn = case mn of
    Nothing -> text "Awaiting data"
    Just n -> span [] <| List.map2
        (viewAtom <| Tuple.first <| .values n)
        (List.map Tuple.second <| .types td)
        (Tuple.second <| .values n)

viewAtom : Maybe Attributee -> AtomDef -> WireValue -> Html a
viewAtom ma def wv =
  let
    castedView : (WireValue -> Result String b) -> (Maybe Attributee -> b -> Html a) -> Html a
    castedView c h = case c wv of
        Ok a -> h ma a
        Err msg -> text msg
  in case def of
    ADEnum opts -> castedView asWord8 <| enumViewer opts
    ADFloat bounds -> castedView asFloat <| floatViewer bounds
    ADString re -> castedView asString <| textViewer re
    ADRef ty -> castedView asString <| refViewer ty
    _ -> text <| "View not implemented: " ++ toString def

viewConstNodeEdit
   : TupleDefinition -> Maybe ConstDataNodeT -> FormState NeConstT
  -> Html (NodeEditEvent NeConstT)
viewConstNodeEdit d mn s = viewConstTupleEdit (List.map Tuple.second <| .types d) (Maybe.map (Tuple.second << .values) mn) s

viewConstTupleEdit
   : List AtomDef -> Maybe (List WireValue) -> FormState NeConstT
   -> Html (NodeEditEvent NeConstT)
viewConstTupleEdit defs mv s =
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
    asPartial idx wv = NeeUpdate <| Result.withDefault editBase <| replaceIdx idx (Just wv) editBase
    atomEditor idx def mwv aes = Html.map (asPartial idx) <| viewAtomEdit def mwv aes
    atomEditors = List.map4 atomEditor (List.range 0 nDefs) defs current atomEditStates
    filledFields = List.filterMap identity editBase
    content = if List.length filledFields == List.length defs
      then button [onClick <| NeeSubmit editBase] [text "Apply"] :: atomEditors
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

enumViewer : List String -> Maybe Attributee -> Int -> Html a
enumViewer opts ma e = text <| case itemAtIndex e opts of
    Just v -> v
    Nothing -> "Enum index out of range"

enumEditor : List String -> Maybe Int -> AtomEditState Int -> Html Int
enumEditor opts me se = text "shuffle"
-- select
--     [onInput (Result.withDefault -1 << String.toInt)]
--     (List.indexedMap (\i o -> option [value (toString i), selected (i == e)] [text o]) opts)
