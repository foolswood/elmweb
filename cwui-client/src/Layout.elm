module Layout exposing (..)

import Set exposing (Set)
import Html exposing (Html)

import Futility exposing (updateIdx)
import Form exposing (FormState(..), formState, FormStore, FormUiEvent, UnboundFui, bindFui, mapUfui)

type alias LayoutPath = List Int

type Layout p
  = LayoutContainer (List (Layout p))
  | LayoutLeaf p

updateLayoutPath : (Layout p -> Result String (Layout p)) -> LayoutPath -> Layout p -> Result String (Layout p)
updateLayoutPath u p l = case p of
    (idx :: leftOver) -> case l of
        LayoutContainer kids -> Result.map LayoutContainer <|
            updateIdx (updateLayoutPath u leftOver) idx kids
        LayoutLeaf _ -> Err "Attempting to update layout path below leaf"
    [] -> u l

setLayout : Layout p -> LayoutPath -> Layout p -> Result String (Layout p)
setLayout v p l = case p of
    (idx :: leftOver) -> case l of
        LayoutContainer kids -> Result.map LayoutContainer <|
            updateIdx (setLayout v leftOver) idx kids
        LayoutLeaf _ -> Err "Attempting to update layout path below leaf"
    [] -> Ok <| v

setLeafBinding : LayoutPath -> p -> Layout p -> Result String (Layout p)
setLeafBinding p tgt l = setLayout (LayoutLeaf tgt) p l

initContainer : LayoutPath -> Layout p -> Result String (Layout p)
initContainer = setLayout <| LayoutContainer []

addLeaf : p -> LayoutPath -> Layout p -> Result String (Layout p)
addLeaf p = updateLayoutPath <| \l -> case l of
    LayoutContainer kids -> Ok <| LayoutContainer <| LayoutLeaf p :: kids
    LayoutLeaf p -> Err "Attempting to add leaf to leaf"

layoutRequires : Layout comparable -> Set comparable
layoutRequires l = case l of
    LayoutContainer kids -> List.foldl (\k acc -> Set.union acc <| layoutRequires k) Set.empty kids
    LayoutLeaf p -> Set.singleton p

-- FIXME: May well be pointless
mapLayout : (a -> b) -> Layout a -> Layout b
mapLayout f l = case l of
    LayoutContainer kids -> LayoutContainer <| List.map (mapLayout f) kids
    LayoutLeaf a -> LayoutLeaf <| f a

-- Html generating:

containerHtml : List (Html a) -> Html a
containerHtml = Html.div []

viewLayout : (p -> Html a) -> Layout p -> Html a
viewLayout h l = case l of
    LayoutContainer kids -> containerHtml <| List.map (viewLayout h) kids
    LayoutLeaf p -> h p

type alias LayoutTargetEditor p = Maybe p -> FormState p -> Html (UnboundFui p)

type LayoutEditEvent p
  = LeeSet p
  | LeeAdd p

updateLayout : LayoutPath -> LayoutEditEvent p -> Layout p -> Result String (Layout p)
updateLayout lp lee l = case lee of
    LeeSet p -> setLeafBinding lp p l
    LeeAdd p -> addLeaf p lp l

getP : LayoutEditEvent p -> p
getP lee = case lee of
    LeeSet p -> p  -- FIXME: is this right?
    LeeAdd p -> p

containerAddControls
   : p -> LayoutTargetEditor p -> LayoutPath -> FormState (LayoutEditEvent p)
  -> Html (FormUiEvent LayoutPath (LayoutEditEvent p))
containerAddControls editInitial lte lp les =
  let
    s = case les of
        FsViewing -> FsEditing editInitial
        FsEditing lee -> FsEditing <| getP lee
        FsPending lee -> FsPending <| getP lee
  in Html.map (bindFui lp << mapUfui LeeAdd) <| lte Nothing s

mapFormState : (a -> b) -> FormState a -> FormState b
mapFormState c s = case s of
    FsViewing -> FsViewing
    FsEditing a -> FsEditing <| c a
    FsPending a -> FsPending <| c a

-- FIXME: 2nd arg of h should be (Maybe p) and editing of containers
viewEditLayout
   : p -> LayoutTargetEditor p -> FormStore LayoutPath (LayoutEditEvent p) -> Layout p
   -> Html (FormUiEvent LayoutPath (LayoutEditEvent p))
viewEditLayout editInitial h fs =
  let
    go lp l =
      let
        s = formState lp fs
      in case l of
        LayoutContainer kids -> containerHtml <|
            containerAddControls editInitial h lp s :: List.indexedMap (\i -> go (lp ++ [i])) kids
        LayoutLeaf p -> Html.map (bindFui lp << mapUfui LeeSet) <| h (Just p) (mapFormState getP s)
  in go []
