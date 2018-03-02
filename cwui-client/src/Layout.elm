module Layout exposing (..)

import Array exposing (Array)
import Set exposing (Set)
import Html exposing (Html)
import Html.Events as Hevt

import Futility exposing (updateIdx)
import Form exposing (FormState(..), formState, FormStore, FormUiEvent, UnboundFui(..), bindFui, mapUfui, castFormState)

type alias LayoutPath = List Int

type Layout p
  = LayoutContainer (Array (Layout p))
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
initContainer = setLayout <| LayoutContainer Array.empty

addLeaf : p -> LayoutPath -> Layout p -> Result String (Layout p)
addLeaf p = updateLayoutPath <| \l -> case l of
    LayoutContainer kids -> Ok <| LayoutContainer <| Array.push (LayoutLeaf p) kids
    LayoutLeaf p -> Err "Attempting to add leaf to leaf"

splitLast : List a -> Maybe (List a, a)
splitLast =
  let
    go acc l = case l of
        (a :: []) -> Just (acc, a)
        (a :: leftOver) -> go (acc ++ [a]) leftOver
        [] -> Nothing
  in go []

removeIdx : Int -> Array a -> Array a
removeIdx idx a = Array.append
    (Array.slice 0 idx a) (Array.slice (idx + 1) (Array.length a) a)

removeSubtree : LayoutPath -> Layout p -> Result String (Layout p)
removeSubtree lp layout =
  let
    removeChild seg l = case l of
        LayoutContainer kids -> Ok <| LayoutContainer <| removeIdx seg kids
        LayoutLeaf _ -> Err "Attempted to remove subtree of leaf"
  in case splitLast lp of
    Just (plp, seg) -> updateLayoutPath (removeChild seg) plp layout
    Nothing -> Err "Attempted to remove layout root"

layoutRequires : Layout comparable -> Set comparable
layoutRequires l = case l of
    LayoutContainer kids -> Array.foldl (\k acc -> Set.union acc <| layoutRequires k) Set.empty kids
    LayoutLeaf p -> Set.singleton p

-- FIXME: May well be pointless
mapLayout : (a -> b) -> Layout a -> Layout b
mapLayout f l = case l of
    LayoutContainer kids -> LayoutContainer <| Array.map (mapLayout f) kids
    LayoutLeaf a -> LayoutLeaf <| f a

-- Html generating:

containerHtml : Array (Html a) -> Html a
containerHtml = Html.div [] << Array.toList

viewLayout : (p -> Html a) -> Layout p -> Html a
viewLayout h l = case l of
    LayoutContainer kids -> containerHtml <| Array.map (viewLayout h) kids
    LayoutLeaf p -> h p

type LayoutEditEvent
  = LeeSet
  | LeeAdd
  | LeeRemove

-- FIXME: Not happy with the LayoutEditEvent in this:
type alias LayoutTargetEditor p = LayoutEditEvent -> Maybe p -> FormState p LayoutEditEvent -> Html (UnboundFui p LayoutEditEvent)

updateLayout : LayoutPath -> LayoutEditEvent -> Maybe p -> Layout p -> Result String (Layout p)
updateLayout lp lee mp l = case lee of
    LeeSet -> case mp of
        Just p -> setLeafBinding lp p l
        Nothing -> Err "No edit value"
    LeeAdd -> case mp of
        Just p -> addLeaf p lp l
        Nothing -> Err "No edit value"
    LeeRemove -> removeSubtree lp l

containerAddControls
   : p -> LayoutTargetEditor p -> LayoutPath -> FormState p LayoutEditEvent
  -> Html (FormUiEvent LayoutPath p LayoutEditEvent)
containerAddControls editInitial lte lp les =
  let
    s = case les of
        FsViewing -> FsEditing editInitial
        FsEditing p -> FsEditing p
        FsPending r mp -> FsPending r mp
  in Html.map (bindFui lp) <| lte LeeAdd Nothing s

viewEditLayout
   : p -> LayoutTargetEditor p -> FormStore LayoutPath p LayoutEditEvent -> Layout p
   -> Html (FormUiEvent LayoutPath p LayoutEditEvent)
viewEditLayout editInitial h fs =
  let
    go lp l =
      let
        s = formState lp fs
        body = case l of
            LayoutContainer kids -> containerHtml <|
                Array.push (containerAddControls editInitial h lp s) <| Array.indexedMap (\i -> go (lp ++ [i])) kids
            LayoutLeaf p -> Html.map (bindFui lp) <| h LeeSet (Just p) s
        removeButton = case lp of
            [] -> Html.text "Layout wide controls here"
            _ -> Html.button [Hevt.onClick <| bindFui lp <| UfAction LeeRemove] [Html.text "Remove"]
      in Html.span [] [removeButton, body]
  in go []
