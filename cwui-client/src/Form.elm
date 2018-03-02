module Form exposing (..)

import Dict exposing (Dict)

import Futility exposing (castMaybe)

type AtomEditState a
  = AesViewing
  | AesUnfilled
  | AesEditing a

castAes : (a -> Result String b) -> AtomEditState a -> Result String (AtomEditState b)
castAes c s = case s of
    AesViewing -> Ok AesViewing
    AesUnfilled -> Ok AesUnfilled
    AesEditing v -> Result.map AesEditing <| c v

type UnboundFui v r
  = UfPartial v
  | UfAction r

mapUfui : (a -> b) -> UnboundFui a r -> UnboundFui b r
mapUfui f e = case e of
    UfPartial a -> UfPartial <| f a
    UfAction r -> UfAction r

type alias FormUiEvent k v r = (k, UnboundFui v r)

bindFui : k -> UnboundFui v r -> FormUiEvent k v r
bindFui k e = (k, e)

type FormState v r
  = FsViewing
  | FsEditing v
  | FsPending r (Maybe v)

castFormState : (a -> Result String b) -> FormState a r -> Result String (FormState b r)
castFormState c f = case f of
    FsViewing -> Ok FsViewing
    FsEditing a -> Result.map FsEditing <| c a
    FsPending r ma -> Result.map (FsPending r) <| castMaybe c ma

type alias FormStore k v r = Dict k (FormState v r)

formStoreEmpty : FormStore k v r
formStoreEmpty = Dict.empty

type FormEvent k v r
  = FeAction k r (Maybe v)
  | FeError String
  | FeNoop

formState : comparable -> FormStore comparable v r -> FormState v r
formState k = Maybe.withDefault FsViewing << Dict.get k

formUiUpdate : FormUiEvent comparable v r -> FormStore comparable v r -> (FormStore comparable v r, FormEvent comparable v r)
formUiUpdate (k, fue) fs = case fue of
    UfPartial v -> (Dict.insert k (FsEditing v) fs, FeNoop)
    UfAction r ->
      let
        (mv, mr) = case formState k fs of
            FsViewing -> (Nothing, Nothing)
            FsEditing v -> (Just v, Nothing)
            FsPending r mv -> (mv, Just r)
      in case mr of
        Nothing -> (Dict.insert k (FsPending r mv) fs, FeAction k r mv)
        Just oldR -> if r == oldR
          then (fs, FeNoop)
          else (fs, FeError "Already a pending action")

formClear : comparable -> FormStore comparable v r -> FormStore comparable v r
formClear = Dict.remove
