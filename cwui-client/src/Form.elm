module Form exposing (..)

import Dict exposing (Dict)

type AtomEditState a
  = AesViewing
  | AesUnfilled
  | AesEditing a

castAes : (a -> Result String b) -> AtomEditState a -> Result String (AtomEditState b)
castAes c s = case s of
    AesViewing -> Ok AesViewing
    AesUnfilled -> Ok AesUnfilled
    AesEditing v -> Result.map AesEditing <| c v

type UnboundFui v
  = UfPartial v
  | UfSubmit

mapUfui : (a -> b) -> UnboundFui a -> UnboundFui b
mapUfui f e = case e of
    UfPartial a -> UfPartial <| f a
    UfSubmit -> UfSubmit

type alias FormUiEvent k v = (k, UnboundFui v)

bindFui : k -> UnboundFui v -> FormUiEvent k v
bindFui k e = (k, e)

type FormState v
  = FsViewing
  | FsEditing v
  | FsPending v

castFormState : (a -> Result String b) -> FormState a -> Result String (FormState b)
castFormState c f = case f of
    FsViewing -> Ok FsViewing
    FsEditing a -> Result.map FsEditing <| c a
    FsPending a -> Result.map FsPending <| c a

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
formUiUpdate (k, fue) fs =
  let
    err msg = (fs, FeError msg)
  in case fue of
    UfPartial v -> (Dict.insert k (FsEditing v) fs, FeNoop)
    UfSubmit -> case Dict.get k fs of
        Nothing -> err "Can't submit: no form state"
        Just s -> case s of
            FsViewing -> err "Can't submit: viewing"
            FsEditing v -> (Dict.insert k (FsPending v) fs, FeSubmit k v)
            FsPending v -> (fs, FeNoop)

formClear : comparable -> FormStore comparable v -> FormStore comparable v
formClear = Dict.remove
