module Form exposing (..)

import Dict exposing (Dict)

type AtomState f e
  = AsViewing f e
  | AsEditing e

castAs : (a -> Result String c) -> (b -> Result String d) -> AtomState a b -> Result String (AtomState c d)
castAs ac bc aes = case aes of
    AsViewing a b -> Result.map2 AsViewing (ac a) (bc b)
    AsEditing b -> Result.map AsEditing <| bc b

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

formInsert : comparable -> Maybe v -> FormStore comparable v -> FormStore comparable v
formInsert k mv fs = case mv of
    Just v -> Dict.insert k (FsEditing v) fs
    Nothing -> Dict.remove k fs

formUpdate
   : comparable -> (FormState v -> FormState v)
  -> FormStore comparable v -> FormStore comparable v
formUpdate k f =
  let
    g v = case f <| Maybe.withDefault FsViewing v of
        FsViewing -> Nothing
        FsEditing a -> Just <| FsEditing a
  in Dict.update k g
