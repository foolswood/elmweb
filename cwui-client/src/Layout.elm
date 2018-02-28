module Layout exposing (..)

import Set exposing (Set)
import Html exposing (Html)

import Futility exposing (updateIdx)
import Form exposing (FormState, formState, FormStore, FormUiEvent)

type alias LayoutPath = List Int

type Layout p
  = LayoutContainer (List (Layout p))
  | LayoutLeaf p

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

-- FIXME: 2nd arg of h should be (Maybe p) and editing of containers
viewEditLayout
   : (LayoutPath -> p -> FormState p -> Html (FormUiEvent LayoutPath p))
   -> FormStore LayoutPath p -> Layout p -> Html (FormUiEvent LayoutPath p)
viewEditLayout h fs =
  let
    go lp l = case l of
        LayoutContainer kids -> containerHtml <| List.indexedMap (\i -> go (lp ++ [i])) kids
        LayoutLeaf p -> h lp p (formState lp fs)
  in go []
