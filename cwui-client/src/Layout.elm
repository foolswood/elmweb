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
  | LayoutDynamic p
  | LayoutLeaf p

updateLayoutPath : (Layout p -> Result String (Layout p)) -> LayoutPath -> Layout p -> Result String (Layout p)
updateLayoutPath u p l = case p of
    (idx :: leftOver) -> case l of
        LayoutContainer kids -> Result.map LayoutContainer <|
            updateIdx (updateLayoutPath u leftOver) idx kids
        LayoutDynamic _ -> Err "Attempted to update dynamic layout"
        LayoutLeaf _ -> Err "Attempting to update layout path below leaf"
    [] -> u l

setLayout : Layout p -> LayoutPath -> Layout p -> Result String (Layout p)
setLayout v = updateLayoutPath <| always <| Ok v

setLeafBinding : LayoutPath -> p -> Layout p -> Result String (Layout p)
setLeafBinding p tgt l = setLayout (LayoutLeaf tgt) p l

initContainer : LayoutPath -> Layout p -> Result String (Layout p)
initContainer = setLayout <| LayoutContainer Array.empty

addLeaf : p -> LayoutPath -> Layout p -> Result String (Layout p)
addLeaf p = updateLayoutPath <| \l -> case l of
    LayoutContainer kids -> Ok <| LayoutContainer <| Array.push (LayoutLeaf p) kids
    _ -> Err "Attempting to add leaf to non-container"

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
        _ -> Err "Attempted to remove subtree of non-container"
  in case splitLast lp of
    Just (plp, seg) -> updateLayoutPath (removeChild seg) plp layout
    Nothing -> Err "Attempted to remove layout root"

layoutRequires : (comparable -> Layout comparable) -> Layout comparable -> Set comparable
layoutRequires dyn l = case l of
    LayoutContainer kids -> Array.foldl (\k acc -> Set.union acc <| layoutRequires dyn k) Set.empty kids
    LayoutDynamic p -> layoutRequires dyn <| dyn p
    LayoutLeaf p -> Set.singleton p

-- FIXME: May well be pointless
mapLayout : (a -> b) -> Layout a -> Layout b
mapLayout f l = case l of
    LayoutContainer kids -> LayoutContainer <| Array.map (mapLayout f) kids
    LayoutDynamic a -> LayoutDynamic <| f a
    LayoutLeaf a -> LayoutLeaf <| f a

-- Html generating:

containerHtml : Array (Html a) -> Html a
containerHtml = Html.div [] << Array.toList

viewLayout : (p -> Layout p) -> (p -> Html a) -> Layout p -> Html a
viewLayout dyn h l = case l of
    LayoutContainer kids -> containerHtml <| Array.map (viewLayout dyn h) kids
    LayoutDynamic p -> viewLayout dyn h <| dyn p
    LayoutLeaf p -> h p

type LayoutEditEvent
  = LeeRemove
  | LeeSetLeaf
  | LeeAddLeaf
  | LeeSetDynamic

-- FIXME: Not happy with the LayoutEditEvent in this:
type alias LayoutTargetEditor p = LayoutEditEvent -> Maybe p -> FormState p LayoutEditEvent -> Html (UnboundFui p LayoutEditEvent)

updateLayout : LayoutPath -> LayoutEditEvent -> Maybe p -> Layout p -> Result String (Layout p)
updateLayout lp lee mp l = case lee of
    LeeRemove -> removeSubtree lp l
    LeeSetLeaf -> case mp of
        Just p -> setLeafBinding lp p l
        Nothing -> Err "No edit value"
    LeeAddLeaf -> case mp of
        Just p -> addLeaf p lp l
        Nothing -> Err "No edit value"
    LeeSetDynamic -> case mp of
        Just p -> setLayout (LayoutDynamic p) lp l
        Nothing -> Err "No edit value"

containerAddControls
   : p -> LayoutTargetEditor p -> LayoutPath -> FormState p LayoutEditEvent
  -> Html (FormUiEvent LayoutPath p LayoutEditEvent)
containerAddControls editInitial lte lp les =
  let
    s = case les of
        FsViewing -> FsEditing editInitial
        FsEditing p -> FsEditing p
        FsPending r mp -> FsPending r mp
  in Html.map (bindFui lp) <| lte LeeAddLeaf Nothing s

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
            LayoutDynamic p -> Html.map (bindFui lp) <| h LeeSetDynamic (Just p) s
            LayoutLeaf p -> Html.map (bindFui lp) <| h LeeSetLeaf (Just p) s
        removeButton = case lp of
            [] -> Html.text "Layout wide controls here"
            _ -> Html.button [Hevt.onClick <| bindFui lp <| UfAction LeeRemove] [Html.text "Remove"]
      in Html.span [] [removeButton, body]
  in go []
