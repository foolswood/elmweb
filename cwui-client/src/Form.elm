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

type FormState v
  = FsViewing
  | FsEditing v

castFormState : (a -> Result String b) -> FormState a -> Result String (FormState b)
castFormState c f = case f of
    FsViewing -> Ok FsViewing
    FsEditing a -> Result.map FsEditing <| c a

type alias FormStore k v = Dict k (FormState v)

formStoreEmpty : FormStore k v
formStoreEmpty = Dict.empty

formState : comparable -> FormStore comparable v -> FormState v
formState k = Maybe.withDefault FsViewing << Dict.get k

formUpdate : comparable -> Maybe v -> FormStore comparable v -> FormStore comparable v
formUpdate k mv fs = case mv of
    Just v -> Dict.insert k (FsEditing v) fs
    Nothing -> Dict.remove k fs
