module Layout exposing (..)

import Futility exposing (updateIdx)

type alias LayoutPath = List Int

type Layout p
  = LayoutContainer (List (Layout p))
  | LayoutLeaf p

setLeafBinding : LayoutPath -> p -> Layout p -> Result String (Layout p)
setLeafBinding p tgt l = case p of
    (idx :: leftOver) -> case l of
        LayoutContainer kids -> Result.map LayoutContainer <|
            updateIdx (setLeafBinding leftOver tgt) idx kids
        LayoutLeaf _ -> Err "Attempting to set leaf below leaf"
    [] -> Ok <| LayoutLeaf tgt

-- FIXME: Common traversal code
initContainer : LayoutPath -> Layout p -> Result String (Layout p)
initContainer p l = case p of
    (idx :: leftOver) -> case l of
        LayoutContainer kids -> Result.map LayoutContainer <|
            updateIdx (initContainer leftOver) idx kids
        LayoutLeaf _ -> Err "Attempting to init container below leaf"
    [] -> Ok <| LayoutContainer []

-- FIXME: May well be pointless
mapLayout : (a -> b) -> Layout a -> Layout b
mapLayout f l = case l of
    LayoutContainer kids -> LayoutContainer <| List.map (mapLayout f) kids
    LayoutLeaf a -> LayoutLeaf <| f a
