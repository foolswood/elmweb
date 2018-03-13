module Layout exposing (..)

import Array exposing (Array)
import Set exposing (Set)
import Html exposing (Html)
import Html.Events as Hevt

import Futility exposing (updateIdx)
import Form exposing (FormState(..), formState, FormStore, formUpdate)

type alias LayoutPath = List Int

type Layout p
  = LayoutContainer (Array (Layout p))
  | LayoutChildChoice p (Layout p)
  | LayoutDynamic p
  | LayoutLeaf p

updateLayoutPath : (Layout p -> Result String (Layout p)) -> LayoutPath -> Layout p -> Result String (Layout p)
updateLayoutPath u p l = case p of
    (idx :: leftOver) -> case l of
        LayoutContainer kids -> Result.map LayoutContainer <|
            updateIdx (updateLayoutPath u leftOver) idx kids
        LayoutChildChoice p kid -> Result.map (LayoutChildChoice p) <| updateLayoutPath u leftOver kid
        LayoutDynamic _ -> Err "Attempted to update dynamic layout"
        LayoutLeaf _ -> Err "Attempting to update layout path below leaf"
    [] -> u l

setLayout : Layout p -> LayoutPath -> Layout p -> Result String (Layout p)
setLayout v = updateLayoutPath <| always <| Ok v

setLeafBinding : LayoutPath -> p -> Layout p -> Result String (Layout p)
setLeafBinding p tgt l = setLayout (LayoutLeaf tgt) p l

setChooser : p -> Layout p -> Result String (Layout p)
setChooser p l = case l of
    LayoutChildChoice _ kid -> Ok <| LayoutChildChoice p kid
    _ -> Err "Not a ChildChoice"

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

layoutRequires
   : (comparable -> comparable -> comparable) -> (comparable -> Layout comparable)
  -> (comparable -> Array comparable) -> Layout comparable -> Set comparable
layoutRequires joinPath dyn cg =
  let
    go l = case l of
        LayoutContainer kids -> Array.foldl (\k acc -> Set.union acc <| go k) Set.empty kids
        LayoutChildChoice p k -> Set.insert p <| go <| expandChildChoice joinPath k <| cg p
        LayoutDynamic p -> go <| dyn p
        LayoutLeaf p -> Set.singleton p
  in go

expandChildChoice : (p -> p -> p) -> Layout p -> Array p -> Layout p
expandChildChoice joinPath kidLayout basePaths =
    LayoutContainer <| Array.map (rebaseLayout joinPath kidLayout) basePaths

rebaseLayout : (p -> p -> p) -> Layout p -> p -> Layout p
rebaseLayout joinPath l basePath = case l of
    LayoutContainer kids -> LayoutContainer <| Array.map (\l -> rebaseLayout joinPath l basePath) kids
    -- Leaves the paths in the child template alone:
    LayoutChildChoice p kid -> LayoutChildChoice (joinPath basePath p) kid
    LayoutDynamic p -> LayoutDynamic <| joinPath basePath p
    LayoutLeaf p -> LayoutLeaf <| joinPath basePath p

-- FIXME: May well be pointless
mapLayout : (a -> b) -> Layout a -> Layout b
mapLayout f l = case l of
    LayoutContainer kids -> LayoutContainer <| Array.map (mapLayout f) kids
    LayoutChildChoice a kid -> LayoutChildChoice (f a) <| mapLayout f kid
    LayoutDynamic a -> LayoutDynamic <| f a
    LayoutLeaf a -> LayoutLeaf <| f a

-- Html generating:

containerHtml : Array (Html a) -> Html a
containerHtml = Html.div [] << Array.toList

viewLayout : (p -> p -> p) -> (p -> Layout p) -> (p -> Array p) -> (p -> Html a) -> Layout p -> Html a
viewLayout joinPath dyn chosenKids h =
  let
    go l = case l of
        LayoutContainer kids -> containerHtml <| Array.map go kids
        LayoutChildChoice p kid -> go <| expandChildChoice joinPath kid <| chosenKids p
        LayoutDynamic p -> go <| dyn p
        LayoutLeaf p -> h p
  in go

type LayoutEditEvent p
  = LeeUpdate p
  | LeeRemove
  | LeeSubdivide
  | LeeSetDynamic p
  | LeeSetChooser p
  | LeeSetLeaf p
  | LeeAddLeaf p

-- FIXME: Not happy with the LayoutEditEvent in this:
type alias LayoutTargetEditor p
   = (p -> LayoutEditEvent p) -> (p -> LayoutEditEvent p)
  -> Maybe p -> FormState p
  -> Html (LayoutEditEvent p)

updateLayout
   : LayoutPath -> LayoutEditEvent p
  -> FormStore LayoutPath p -> Layout p
  -> Result String (FormStore LayoutPath p, Layout p)
updateLayout lp lee fs l =
  let
    (newState, rNewLayout) = case lee of
        LeeUpdate p -> (Just p, Ok l)
        LeeRemove -> (Nothing, removeSubtree lp l)
        LeeSubdivide -> (Nothing, updateLayoutPath (Ok << LayoutContainer << Array.repeat 1) lp l)
        LeeSetChooser p -> (Nothing, updateLayoutPath (setChooser p) lp l)
        LeeSetDynamic p -> (Nothing, setLayout (LayoutDynamic p) lp l)
        LeeSetLeaf p -> (Nothing, setLeafBinding lp p l)
        LeeAddLeaf p -> (Nothing, addLeaf p lp l)
    newFs = formUpdate lp newState fs
  in Result.map ((,) newFs) rNewLayout

containerAddControls
   : p -> LayoutTargetEditor p -> LayoutPath -> FormState p
  -> Html (LayoutPath, LayoutEditEvent p)
containerAddControls editInitial lte lp les =
  let
    s = case les of
        FsViewing -> FsEditing editInitial
        FsEditing p -> FsEditing p
  in Html.map ((,) lp) <| lte LeeUpdate LeeAddLeaf Nothing s

viewEditLayout
   : p -> LayoutTargetEditor p -> FormStore LayoutPath p -> Layout p
   -> Html (LayoutPath, LayoutEditEvent p)
viewEditLayout editInitial h fs =
  let
    go contained lp l =
      let
        bindEvt = Html.map ((,) lp)
        s = formState lp fs
        (typeControls, body) = case l of
            LayoutContainer kids ->
              let
                c = [containerAddControls editInitial h lp s]
                b = containerHtml <| Array.indexedMap (\i -> go True (lp ++ [i])) kids
              in (c, b)
            LayoutChildChoice p kid ->
              let
                b = Html.div []
                  [ bindEvt <| h LeeUpdate LeeSetChooser (Just p) s
                  , go False (lp ++ [0]) kid
                  ]
              in ([], b)
            LayoutDynamic p -> ([], bindEvt <| h LeeUpdate LeeSetDynamic (Just p) s)
            LayoutLeaf p -> ([], bindEvt <| h LeeUpdate LeeSetLeaf (Just p) s)
        layoutControls = if contained
            then Html.button [Hevt.onClick <| (lp, LeeRemove)] [Html.text "Remove"] :: typeControls
            else typeControls
        alwaysControls = [Html.button [Hevt.onClick <| (lp, LeeSubdivide)] [Html.text "Split"]]
      in Html.div [] <| alwaysControls ++ layoutControls ++ [body]
  in go False []
