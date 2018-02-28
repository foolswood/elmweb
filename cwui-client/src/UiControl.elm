module UiControl exposing (..)

import Dict exposing (Dict)

import ClTypes exposing (Path)
import Futility exposing (itemAtIndex)

type FormUiEvent k v
  = FuePartial k v
  | FueSubmit k

type FormState v
  = FsViewing
  | FsEditing v
  | FsPending v

type alias FormStore k v = Dict k (FormState v)

formStoreEmpty : FormStore k v
formStoreEmpty = Dict.empty

type FormEvent k v
  = FeSubmit k v
  | FeError String
  | FeNoop

formState : comparable -> FormStore comparable v -> FormState v
formState k = Maybe.withDefault FsViewing << Dict.get k

formUiUpdate : FormUiEvent comparable v -> FormStore comparable v -> (FormStore comparable v, FormEvent comparable v)
formUiUpdate fe fs =
  let
    err msg = (fs, FeError msg)
  in case fe of
    FuePartial k v -> (Dict.insert k (FsEditing v) fs, FeNoop)
    FueSubmit k -> case Dict.get k fs of
        Nothing -> err "Can't submit: no form state"
        Just s -> case s of
            FsViewing -> err "Can't submit: viewing"
            FsEditing v -> (Dict.insert k (FsPending v) fs, FeSubmit k v)
            FsPending v -> (fs, FeNoop)

formClear : comparable -> FormStore comparable v -> FormStore comparable v
formClear = Dict.remove

-- FIXME: Useful?
updateIdx : (a -> Result String a) -> Int -> List a -> Result String (List a)
updateIdx f idx l =
    Result.map ((++) <| List.take idx l) <| case List.drop idx l of
        (oldA :: leftOver) -> Result.map (\newA -> newA :: leftOver) <| f oldA
        [] -> Err "Index out of range"

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

-- FIXME: May well be pointless
mapLayout : (a -> b) -> Layout a -> Layout b
mapLayout f l = case l of
    LayoutContainer kids -> LayoutContainer <| List.map (mapLayout f) kids
    LayoutLeaf a -> LayoutLeaf <| f a

-- toFormState : Definition -> Node -> Result String FormState
