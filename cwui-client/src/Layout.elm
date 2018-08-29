module Layout exposing (..)

import Array exposing (Array)
import Set exposing (Set)
import Html as H exposing (Html)
import Html.Events as Hevt

import Cmp.Cmp exposing (Cmp)
import Cmp.Set as CSet exposing (CmpSet)
import Futility exposing (updateIdx)
import Form exposing (FormState(..), formState, FormStore, formInsert)
import EditTypes exposing (EditEvent(..), mapEe)

type Layout p q s
  = LayoutContainer (Array (Layout p q s))
  | LayoutChildChoice p (Layout q q s)
  | LayoutDynamic p
  | LayoutLeaf p
  | LayoutSpecial s

type ConcreteLayout p s
  = ClContainer (Array (ConcreteLayout p s))
  | ClLeaf p
  | ClSpecial s

cementLayout : (p -> q -> p) -> (p -> Layout q q s) -> (p -> Array q) -> Layout p q s -> ConcreteLayout p s
cementLayout join dyn getChildPaths =
  let
    goPartial : p -> Layout q q s -> ConcreteLayout p s
    goPartial p l = case l of
        LayoutContainer kids -> ClContainer <| Array.map (goPartial p) kids
        LayoutChildChoice q subL -> let ccp = join p q in
            goFull <| expandChildChoice join subL <| Array.map (join ccp) <| getChildPaths <| join ccp q
        LayoutDynamic q -> goPartial (join p q) <| dyn (join p q)
        LayoutLeaf q -> ClLeaf <| join p q
        LayoutSpecial s -> ClSpecial s
    goFull : Layout p q s -> ConcreteLayout p s
    goFull l = case l of
        LayoutContainer kids -> ClContainer <| Array.map goFull kids
        LayoutChildChoice p subL -> goFull <| expandChildChoice join subL <| Array.map (join p) <| getChildPaths p
        LayoutDynamic p -> goPartial p <| dyn p
        LayoutLeaf p -> ClLeaf p
        LayoutSpecial s -> ClSpecial s
  in goFull

expandChildChoice : (p -> q -> p) -> Layout q q s -> Array p -> Layout p q s
expandChildChoice joinPath kidLayout basePaths =
    LayoutContainer <| Array.map (rebaseLayout joinPath kidLayout) basePaths

rebaseLayout : (p -> q -> p) -> Layout q q s -> p -> Layout p q s
rebaseLayout joinPath l basePath = case l of
    LayoutContainer kids -> LayoutContainer <| Array.map (\l -> rebaseLayout joinPath l basePath) kids
    -- Leaves the paths in the child template alone:
    LayoutChildChoice p kid -> LayoutChildChoice (joinPath basePath p) kid
    LayoutDynamic p -> LayoutDynamic <| joinPath basePath p
    LayoutLeaf p -> LayoutLeaf <| joinPath basePath p
    LayoutSpecial s -> LayoutSpecial s  -- FIXME: Should this have an option to do something, or error?

-- removeIdx : Int -> Array a -> Array a
-- removeIdx idx a = Array.append
--     (Array.slice 0 idx a) (Array.slice (idx + 1) (Array.length a) a)
-- 
-- layoutRequires
--    : Cmp p comparable -> (p -> p -> p) -> (p -> Layout p s)
--   -> (p -> Array p) -> (s -> CmpSet p comparable) -> Layout p s
--   -> CmpSet p comparable
-- layoutRequires cmp joinPath dyn cg specialRequires =
--   let
--     go l = case l of
--         LayoutContainer kids -> Array.foldl
--             (\k acc -> CSet.union acc <| go k) (CSet.empty cmp) kids
--         LayoutChildChoice p k ->
--             CSet.insert p <| go <| expandChildChoice joinPath k <| cg p
--         LayoutDynamic p -> go <| dyn p
--         LayoutLeaf p -> CSet.singleton cmp p
--         LayoutSpecial s -> specialRequires s
--   in go
-- 
-- -- Html generating:
-- 
-- containerHtml : Array (Html a) -> Html a
-- containerHtml = H.div [] << Array.toList
-- 
-- viewLayout
--    : (p -> p -> p) -> (p -> Layout p s) -> (p -> Array p) -> (p -> Html a)
--    -> (s -> Html a) -> Layout p s -> Html a
-- viewLayout joinPath dyn chosenKids h viewSpecial =
--   let
--     go l = case l of
--         LayoutContainer kids -> containerHtml <| Array.map go kids
--         LayoutChildChoice p kid -> go <| expandChildChoice joinPath kid <| chosenKids p
--         LayoutDynamic p -> go <| dyn p
--         LayoutLeaf p -> h p
--         LayoutSpecial s -> viewSpecial s
--   in go
-- 
-- type LayoutEvent p s
--   = LeeRemove
--   | LeeSubdivide
--   | LeeSetDynamic p
--   | LeeSetChooser p
--   | LeeSetLeaf p
--   | LeeAddLeaf p
--   | LeeSetSpecial s
-- 
-- type alias LayoutTargetEditor p = Maybe p -> FormState p -> Html (EditEvent p p)
-- 
-- updateLayout
--    : LayoutPath -> EditEvent p (LayoutEvent p s)
--   -> FormStore LayoutPath p -> Layout p s
--   -> Result String (FormStore LayoutPath p, Layout p s)
-- updateLayout lp lee fs l =
--   let
--     (newState, rNewLayout) = case lee of
--         EeUpdate p -> (Just p, Ok l)
--         EeSubmit evt -> (,) Nothing <| case evt of
--             LeeRemove -> removeSubtree lp l
--             LeeSubdivide -> updateLayoutPath (Ok << LayoutContainer << Array.repeat 1) lp l
--             LeeSetChooser p -> updateLayoutPath (setChooser p) lp l
--             LeeSetDynamic p -> setLayout (LayoutDynamic p) lp l
--             LeeSetLeaf p -> setLeafBinding lp p l
--             LeeAddLeaf p -> addLeaf p lp l
--             LeeSetSpecial sp -> setLayout (LayoutSpecial sp) lp l
--     newFs = formInsert lp newState fs
--   in Result.map ((,) newFs) rNewLayout
-- 
-- mapSubmitEvt : (c -> d) -> EditEvent a c -> EditEvent a d
-- mapSubmitEvt f e = mapEe identity f e
-- 
-- containerAddControls
--    : p -> LayoutTargetEditor p -> LayoutPath -> FormState p
--   -> Html (LayoutPath, EditEvent p (LayoutEvent p s))
-- containerAddControls editInitial lte lp les =
--   let
--     s = case les of
--         FsViewing -> FsEditing editInitial
--         FsEditing p -> FsEditing p
--   in H.map ((,) lp << mapSubmitEvt LeeAddLeaf) <| lte Nothing s
-- 
-- viewEditLayout
--    : p -> LayoutTargetEditor p -> (s -> Html s) -> FormStore LayoutPath p -> Layout p s
--    -> Html (LayoutPath, EditEvent p (LayoutEvent p s))
-- viewEditLayout editInitial h specialEdit fs =
--   let
--     go contained lp l =
--       let
--         bindEvt f = H.map ((,) lp << mapSubmitEvt f)
--         s = formState lp fs
--         (typeControls, body) = case l of
--             LayoutContainer kids ->
--               let
--                 c = [containerAddControls editInitial h lp s]
--                 b = containerHtml <| Array.indexedMap (\i -> go True (lp ++ [i])) kids
--               in (c, b)
--             LayoutChildChoice p kid ->
--               let
--                 b = H.div []
--                   [ bindEvt LeeSetChooser <| h (Just p) s
--                   , go False (lp ++ [0]) kid
--                   ]
--               in ([], b)
--             LayoutDynamic p -> ([], bindEvt LeeSetDynamic <| h (Just p) s)
--             LayoutLeaf p -> ([], bindEvt LeeSetLeaf <| h (Just p) s)
--             LayoutSpecial sp -> ([], H.map ((,) lp << EeSubmit << LeeSetSpecial) <| specialEdit sp)
--         layoutControls = if contained
--             then H.button [Hevt.onClick <| (lp, EeSubmit LeeRemove)] [H.text "Remove"] :: typeControls
--             else typeControls
--         alwaysControls = [H.button [Hevt.onClick <| (lp, EeSubmit LeeSubdivide)] [H.text "Split"]]
--       in H.div [] <| alwaysControls ++ layoutControls ++ [body]
--   in go False []
