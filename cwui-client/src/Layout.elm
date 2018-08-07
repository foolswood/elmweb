module Layout exposing (..)

import Array exposing (Array)
import Set exposing (Set)
import Html as H exposing (Html)
import Html.Events as Hevt

import Futility exposing (updateIdx)
import Form exposing (FormState(..), formState, FormStore, formInsert)
import EditTypes exposing (EditEvent(..), mapEe)

type alias LayoutPath = List Int

type Layout p s
  = LayoutContainer (Array (Layout p s))
  | LayoutChildChoice p (Layout p s)
  | LayoutDynamic p
  | LayoutLeaf p
  | LayoutSpecial s

updateLayoutPath : (Layout p s -> Result String (Layout p s)) -> LayoutPath -> Layout p s -> Result String (Layout p s)
updateLayoutPath u p l = case p of
    (idx :: leftOver) -> case l of
        LayoutContainer kids -> Result.map LayoutContainer <|
            updateIdx (updateLayoutPath u leftOver) idx kids
        LayoutChildChoice p kid -> Result.map (LayoutChildChoice p) <| updateLayoutPath u leftOver kid
        LayoutDynamic _ -> Err "Attempted to update dynamic layout"
        LayoutLeaf _ -> Err "Attempting to update layout path below leaf"
        LayoutSpecial _ -> Err "Attempting to update layout path below special"
    [] -> u l

setLayout : Layout p s -> LayoutPath -> Layout p s -> Result String (Layout p s)
setLayout v = updateLayoutPath <| always <| Ok v

setLeafBinding : LayoutPath -> p -> Layout p s -> Result String (Layout p s)
setLeafBinding p tgt l = setLayout (LayoutLeaf tgt) p l

setChooser : p -> Layout p s -> Result String (Layout p s)
setChooser p l = case l of
    LayoutChildChoice _ kid -> Ok <| LayoutChildChoice p kid
    _ -> Err "Not a ChildChoice"

initContainer : LayoutPath -> Layout p s -> Result String (Layout p s)
initContainer = setLayout <| LayoutContainer Array.empty

addLeaf : p -> LayoutPath -> Layout p s -> Result String (Layout p s)
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

removeSubtree : LayoutPath -> Layout p s -> Result String (Layout p s)
removeSubtree lp layout =
  let
    removeChild seg l = case l of
        LayoutContainer kids -> Ok <| LayoutContainer <| removeIdx seg kids
        _ -> Err "Attempted to remove subtree of non-container"
  in case splitLast lp of
    Just (plp, seg) -> updateLayoutPath (removeChild seg) plp layout
    Nothing -> Err "Attempted to remove layout root"

layoutRequires
   : (comparable -> comparable -> comparable) -> (comparable -> Layout comparable s)
  -> (comparable -> Array comparable) -> (s -> Set comparable) -> Layout comparable s -> Set comparable
layoutRequires joinPath dyn cg specialRequires =
  let
    go l = case l of
        LayoutContainer kids -> Array.foldl (\k acc -> Set.union acc <| go k) Set.empty kids
        LayoutChildChoice p k -> Set.insert p <| go <| expandChildChoice joinPath k <| cg p
        LayoutDynamic p -> go <| dyn p
        LayoutLeaf p -> Set.singleton p
        LayoutSpecial s -> specialRequires s
  in go

-- FIXME: This should allow layouts using relative paths
expandChildChoice : (p -> p -> p) -> Layout p s -> Array p -> Layout p s
expandChildChoice joinPath kidLayout basePaths =
    LayoutContainer <| Array.map (rebaseLayout joinPath kidLayout) basePaths

rebaseLayout : (p -> p -> p) -> Layout p s -> p -> Layout p s
rebaseLayout joinPath l basePath = case l of
    LayoutContainer kids -> LayoutContainer <| Array.map (\l -> rebaseLayout joinPath l basePath) kids
    -- Leaves the paths in the child template alone:
    LayoutChildChoice p kid -> LayoutChildChoice (joinPath basePath p) kid
    LayoutDynamic p -> LayoutDynamic <| joinPath basePath p
    LayoutLeaf p -> LayoutLeaf <| joinPath basePath p
    LayoutSpecial s -> LayoutSpecial s  -- FIXME: Should this have an option to do something, or error?

-- FIXME: May well be pointless
mapLayout : (a -> c) -> (b -> d) -> Layout a b -> Layout c d
mapLayout f g l = case l of
    LayoutContainer kids -> LayoutContainer <| Array.map (mapLayout f g) kids
    LayoutChildChoice a kid -> LayoutChildChoice (f a) <| mapLayout f g kid
    LayoutDynamic a -> LayoutDynamic <| f a
    LayoutLeaf a -> LayoutLeaf <| f a
    LayoutSpecial s -> LayoutSpecial <| g s

-- Html generating:

containerHtml : Array (Html a) -> Html a
containerHtml = H.div [] << Array.toList

viewLayout
   : (p -> p -> p) -> (p -> Layout p s) -> (p -> Array p) -> (p -> Html a)
   -> (s -> Html a) -> Layout p s -> Html a
viewLayout joinPath dyn chosenKids h viewSpecial =
  let
    go l = case l of
        LayoutContainer kids -> containerHtml <| Array.map go kids
        LayoutChildChoice p kid -> go <| expandChildChoice joinPath kid <| chosenKids p
        LayoutDynamic p -> go <| dyn p
        LayoutLeaf p -> h p
        LayoutSpecial s -> viewSpecial s
  in go

type LayoutEvent p s
  = LeeRemove
  | LeeSubdivide
  | LeeSetDynamic p
  | LeeSetChooser p
  | LeeSetLeaf p
  | LeeAddLeaf p
  | LeeSetSpecial s

type alias LayoutTargetEditor p = Maybe p -> FormState p -> Html (EditEvent p p)

updateLayout
   : LayoutPath -> EditEvent p (LayoutEvent p s)
  -> FormStore LayoutPath p -> Layout p s
  -> Result String (FormStore LayoutPath p, Layout p s)
updateLayout lp lee fs l =
  let
    (newState, rNewLayout) = case lee of
        EeUpdate p -> (Just p, Ok l)
        EeSubmit evt -> (,) Nothing <| case evt of
            LeeRemove -> removeSubtree lp l
            LeeSubdivide -> updateLayoutPath (Ok << LayoutContainer << Array.repeat 1) lp l
            LeeSetChooser p -> updateLayoutPath (setChooser p) lp l
            LeeSetDynamic p -> setLayout (LayoutDynamic p) lp l
            LeeSetLeaf p -> setLeafBinding lp p l
            LeeAddLeaf p -> addLeaf p lp l
            LeeSetSpecial sp -> setLayout (LayoutSpecial sp) lp l
    newFs = formInsert lp newState fs
  in Result.map ((,) newFs) rNewLayout

mapSubmitEvt : (c -> d) -> EditEvent a c -> EditEvent a d
mapSubmitEvt f e = mapEe identity f e

containerAddControls
   : p -> LayoutTargetEditor p -> LayoutPath -> FormState p
  -> Html (LayoutPath, EditEvent p (LayoutEvent p s))
containerAddControls editInitial lte lp les =
  let
    s = case les of
        FsViewing -> FsEditing editInitial
        FsEditing p -> FsEditing p
  in H.map ((,) lp << mapSubmitEvt LeeAddLeaf) <| lte Nothing s

viewEditLayout
   : p -> LayoutTargetEditor p -> (s -> Html s) -> FormStore LayoutPath p -> Layout p s
   -> Html (LayoutPath, EditEvent p (LayoutEvent p s))
viewEditLayout editInitial h specialEdit fs =
  let
    go contained lp l =
      let
        bindEvt f = H.map ((,) lp << mapSubmitEvt f)
        s = formState lp fs
        (typeControls, body) = case l of
            LayoutContainer kids ->
              let
                c = [containerAddControls editInitial h lp s]
                b = containerHtml <| Array.indexedMap (\i -> go True (lp ++ [i])) kids
              in (c, b)
            LayoutChildChoice p kid ->
              let
                b = H.div []
                  [ bindEvt LeeSetChooser <| h (Just p) s
                  , go False (lp ++ [0]) kid
                  ]
              in ([], b)
            LayoutDynamic p -> ([], bindEvt LeeSetDynamic <| h (Just p) s)
            LayoutLeaf p -> ([], bindEvt LeeSetLeaf <| h (Just p) s)
            LayoutSpecial sp -> ([], H.map ((,) lp << EeSubmit << LeeSetSpecial) <| specialEdit sp)
        layoutControls = if contained
            then H.button [Hevt.onClick <| (lp, EeSubmit LeeRemove)] [H.text "Remove"] :: typeControls
            else typeControls
        alwaysControls = [H.button [Hevt.onClick <| (lp, EeSubmit LeeSubdivide)] [H.text "Split"]]
      in H.div [] <| alwaysControls ++ layoutControls ++ [body]
  in go False []
