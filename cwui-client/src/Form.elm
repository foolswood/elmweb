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

-- FIXME: Other one is a wrapper around this really?
type UnboundFui v
  = UfPartial v
  | UfSubmit

mapUfui : (a -> b) -> UnboundFui a -> UnboundFui b
mapUfui f e = case e of
    UfPartial a -> UfPartial <| f a
    UfSubmit -> UfSubmit

type FormUiEvent k v
  = FuePartial k v
  | FueSubmit k

bindFui : k -> UnboundFui v -> FormUiEvent k v
bindFui k e = case e of
    UfPartial v -> FuePartial k v
    UfSubmit -> FueSubmit k

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
