module UiControl exposing (..)

import Dict exposing (Dict)

import ClTypes exposing (Path)
import Futility exposing (itemAtIndex)

type alias FormPath = List Int

type EntryState
  = EsText String

type alias FormState = List EntryState

type FormWidgetEvent
  = FwUpdate FormPath FormState
  | FwSubmit FormPath

type FormEvent extIdx
  = FormUpdate extIdx FormState
  | FormError String
  | FormNoop

type FormView extIdx = FormView extIdx FormState (List (FormView extIdx))

updateIdx : (a -> Result String a) -> Int -> List a -> Result String (List a)
updateIdx f idx l =
    Result.map ((++) <| List.take idx l) <| case List.drop idx l of
        (oldA :: leftOver) -> Result.map (\newA -> newA :: leftOver) <| f oldA
        [] -> Err "Index out of range"

widgetUpdate : FormWidgetEvent -> FormView extIdx -> (FormView extIdx, FormEvent extIdx)
widgetUpdate evt fv = case evt of
    FwUpdate fp fs ->
      let
        updateFormView p (FormView extIdx parentFs children) = case p of
            (idx :: subP) -> Result.map (FormView extIdx parentFs) <|
                updateIdx (updateFormView subP) idx children
            [] -> Ok <| FormView extIdx fs children
      in case updateFormView fp fv of
        Ok newFv -> (newFv, FormNoop)
        Err msg -> (fv, FormError msg)
    FwSubmit fp ->
      let
        getFormState p (FormView extIdx fs children) = case p of
            (idx :: subP) -> Maybe.andThen (getFormState subP) <|
                itemAtIndex idx children
            [] -> Just (extIdx, fs)
      in case getFormState fp fv of
        Just (extIdx, fs) -> (fv, FormUpdate extIdx fs)
        Nothing -> (fv, FormError "Bad submit path")


-- How would above be useful?

toFormState : Definition -> Node -> Result String FormState

-- Dumping ground:

replaceIdx : Int -> a -> List a -> Result String (List a)
replaceIdx idx v l = if List.length l > idx
  then Ok <| List.take idx l ++ v :: List.drop (idx + 1) l
  else Err "Index out of range"
