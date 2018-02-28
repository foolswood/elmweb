module Form exposing (..)

import Dict exposing (Dict)

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
