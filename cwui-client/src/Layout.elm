module Layout exposing (..)

import Futility exposing (updateIdx)

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

-- FIXME: May well be pointless
mapLayout : (a -> b) -> Layout a -> Layout b
mapLayout f l = case l of
    LayoutContainer kids -> LayoutContainer <| List.map (mapLayout f) kids
    LayoutLeaf a -> LayoutLeaf <| f a
