module UiControl exposing (..)

import ClTypes exposing (Path)
import Futility exposing (itemAtIndex)

type alias FormPath = List Int

type EntryState
  = EsText String

type alias FormState = List EntryState

type FormWidgetEvent
  = FwUpdate FormPath FormState
  | FwSubmit FormPath
  | FwError String

type FormEvent extIdx
  = FormUpdate extIdx FormState
  | FormError String
  | FormNoop

type FormView extIdx
  = FormWidget extIdx FormState
  | FormContainer (List (FormView extIdx))

-- FIXME: good or bad idea?
mapFormView : (a -> b) -> FormView a -> FormView b
mapFormView f v = case v of
    FormWidget a fs -> FormWidget (f a) fs
    FormContainer kids -> FormContainer <| List.map (mapFormView f) kids

updateIdx : (a -> Result String a) -> Int -> List a -> Result String (List a)
updateIdx f idx l =
    Result.map ((++) <| List.take idx l) <| case List.drop idx l of
        (oldA :: leftOver) -> Result.map (\newA -> newA :: leftOver) <| f oldA
        [] -> Err "Index out of range"

widgetUpdate : FormWidgetEvent -> FormView extIdx -> (FormView extIdx, FormEvent extIdx)
widgetUpdate evt fv = case evt of
    FwUpdate fp fs ->
      let
        updateFormView p v = case p of
            (idx :: subP) -> case v of
                FormWidget _ _ -> Err "Attempted to update child of leaf"
                FormContainer children -> Result.map FormContainer <|
                    updateIdx (updateFormView subP) idx children
            [] -> case v of
                FormWidget extIdx _ -> Ok <| FormWidget extIdx fs
                FormContainer _ -> Err "Attempted to set value of container"
      in case updateFormView fp fv of
        Ok newFv -> (newFv, FormNoop)
        Err msg -> (fv, FormError msg)
    FwSubmit fp ->
      let
        getFormState p v = case p of
            (idx :: subP) -> case v of
                FormWidget _ _ -> Nothing
                FormContainer children -> Maybe.andThen (getFormState subP) <|
                    itemAtIndex idx children
            [] -> case v of
                FormWidget extIdx fs -> Just (extIdx, fs)
                FormContainer _ -> Nothing
      in case getFormState fp fv of
        Just (extIdx, fs) -> (fv, FormUpdate extIdx fs)
        Nothing -> (fv, FormError "Bad submit path")
    FwError msg -> (fv, FormError msg)


-- How would above be useful?

type alias LayoutPath = List Int

type Layout p
  = LayoutContainer (List (Layout p))
  | LayoutLeaf p

setLeafBinding : LayoutPath -> p -> Layout p -> Result String (Layout p)
setLeafBinding p tgt l = case p of
    (idx :: leftOver) -> case l of
        LayoutContainer kids -> Result.map LayoutContainer <|
            updateIdx (setLeafBinding leftOver tgt) idx kids
        LayoutLeaf _ -> Err "Attempting to set leaf below leaf"
    [] -> Ok <| LayoutLeaf tgt

-- FIXME: Common traversal code
initContainer : LayoutPath -> Layout p -> Result String (Layout p)
initContainer p l = case p of
    (idx :: leftOver) -> case l of
        LayoutContainer kids -> Result.map LayoutContainer <|
            updateIdx (initContainer leftOver) idx kids
        LayoutLeaf _ -> Err "Attempting to init container below leaf"
    [] -> Ok <| LayoutContainer []

layoutEditorForm : (LayoutPath -> p -> FormView LayoutPath) -> Layout p -> FormView LayoutPath
layoutEditorForm f =
  let
    go lp l = case l of
        LayoutContainer kids ->
            FormContainer <| List.indexedMap (\i -> go (lp ++ [i])) kids
        LayoutLeaf cp -> f lp cp
  in go []

-- FIXME: May well be pointless
mapLayout : (a -> b) -> Layout a -> Layout b
mapLayout f l = case l of
    LayoutContainer kids -> LayoutContainer <| List.map (mapLayout f) kids
    LayoutLeaf a -> LayoutLeaf <| f a

-- toFormState : Definition -> Node -> Result String FormState
