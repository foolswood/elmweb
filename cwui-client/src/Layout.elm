module Layout exposing (..)

import Dict exposing (Dict)
import Array exposing (Array)
import Set exposing (Set)
import Html as H exposing (Html)
import Html.Events as HE
import Html.Attributes as HA

import Cmp.Cmp exposing (Cmp)
import Cmp.Set as CSet exposing (CmpSet)
import Futility exposing (updateIdx, getWithDefault)
import Form exposing (FormState(..), formState, FormStore, formInsert)
import EditTypes exposing (EditEvent(..), mapEe)

type alias ChildSourceSeg = Int
type alias ChildSourceStateSeg = String
type alias DataSourceSeg = Float

type alias ChildSourceId = List ChildSourceSeg
type alias ChildSourceStateId = List ChildSourceStateSeg
type alias DataSourceId = List DataSourceSeg

type BoundLayout
  = BlContainer ChildSourceId
  | BlChildControl DataSourceId ChildSourceStateId
  | BlView DataSourceId

type ConcreteBoundLayout
  = CblContainer (List ConcreteBoundLayout)
  | CblChildControl DataSourceId ChildSourceStateId
  | CblView DataSourceId

cement : (ChildSourceId -> List BoundLayout) -> BoundLayout -> ConcreteBoundLayout
cement expandContainer l = case l of
    BlContainer csid -> CblContainer <| List.map (cement expandContainer) <| expandContainer csid
    BlChildControl dsid csid -> CblChildControl dsid csid
    BlView dsid -> CblView dsid

view
   : (DataSourceId -> Html a) -> (DataSourceId -> ChildSourceStateId -> Html a)
  -> ConcreteBoundLayout -> Html a
view viewData viewChildCtl =
  let
    go cl = case cl of
        CblContainer subLs -> H.div [] <| List.map go subLs
        CblChildControl dsid csid -> viewChildCtl dsid csid
        CblView dsid -> viewData dsid
  in go

type Pattern a b
  = PatternPrefix b (Pattern a b)
  | PatternSubstitute
  | PatternDone

applyPattern : (b -> a -> a) -> b -> Pattern a b -> a -> a
applyPattern join b =
  let
    go p a = case p of
        PatternPrefix prefix subP -> go subP <| join prefix a
        PatternSubstitute -> join b a
        PatternDone -> a
  in go

type ChildSource a
  = CsFixed (List BoundLayout)
  | CsTemplate
        ChildSourceStateId
        (Pattern ChildSourceId ChildSourceSeg)
        (Pattern ChildSourceStateId ChildSourceStateSeg)
        (Pattern DataSourceId DataSourceSeg)
        BoundLayout
  | CsDynamic a

type alias ChildSources a = Dict ChildSourceId (ChildSource a)

resolveChild
   : (ChildSourceStateId -> List (ChildSourceSeg, ChildSourceStateSeg, DataSourceSeg))
  -> (a -> List BoundLayout) -> ChildSources a
  -> ChildSourceId -> List BoundLayout
resolveChild getSegs resolveDynamic childSources csid =
    case getWithDefault (CsFixed []) csid childSources of
        CsFixed subLayouts -> subLayouts
        CsDynamic a -> resolveDynamic a
        CsTemplate cssid csp cssp dsp subLayout ->
          let
            applyPatterns (css, csss, dss) = rebaseBl
                (applyPattern lAppend css csp) (applyPattern lAppend csss cssp)
                (applyPattern lAppend dss dsp) subLayout
          in List.map applyPatterns <| getSegs cssid

lAppend : a -> List a -> List a
lAppend a l = l ++ [a]

rebaseBl
   : (ChildSourceId -> ChildSourceId)
  -> (ChildSourceStateId -> ChildSourceStateId)
  -> (DataSourceId -> DataSourceId)
  -> BoundLayout -> BoundLayout
rebaseBl csr cssr dsr bl = case bl of
    BlContainer csid -> BlContainer <| csr csid
    BlChildControl dsid cssid -> BlChildControl (dsr dsid) (cssr cssid)
    BlView dsid -> BlView <| dsr dsid

edit : (a -> Html a) -> ChildSources a -> BoundLayout -> Html (BoundLayout, ChildSources a)
edit dynEdit childSources =
  let
    go bl = H.div []
      [ H.map (\newBl -> (newBl, childSources)) <| H.select [HE.onInput emptyBlFromStr] <| blStrOpts bl
      , case bl of
            BlContainer csid -> H.div []
              [ H.map (\newCsid -> (BlContainer newCsid, childSources)) <| editCsid csid
              , H.map
                  (\(newItem, newSources) -> (BlContainer csid, Dict.insert csid newItem newSources))
                  <| case getWithDefault (CsFixed []) csid childSources of
                    CsFixed subLayouts -> H.map
                        (Tuple.mapFirst CsFixed)
                        <| H.div [] <| listEdit go subLayouts
                    CsDynamic a -> H.map (\newA -> (CsDynamic a, childSources)) <| dynEdit a
                    CsTemplate cssid csPat cssPat dsPat subL -> H.div []
                      [ H.map
                          (\newCssid -> (CsTemplate newCssid csPat cssPat dsPat subL, childSources))
                          <| editCssid cssid
                      , H.map
                          (\newCsPat -> (CsTemplate cssid newCsPat cssPat dsPat subL, childSources))
                          <| editPattern csPat
                      , H.map
                          (\newCssPat -> (CsTemplate cssid csPat newCssPat dsPat subL, childSources))
                          <| editPattern cssPat
                      , H.map
                          (\newDsPat -> (CsTemplate cssid csPat cssPat newDsPat subL, childSources))
                          <| editPattern dsPat
                      , H.map
                          (\(newSubL, newSources) -> (CsTemplate cssid csPat cssPat dsPat newSubL, newSources))
                          <| go subL
                      ]
              ]
            BlChildControl dsid cssid -> H.div []
              [ H.map (\newDsid -> (BlChildControl newDsid cssid, childSources)) <| editDsid dsid
              , H.map (\newCssid -> (BlChildControl dsid newCssid, childSources)) <| editCssid cssid
              ]
            BlView dsid -> H.map (\newDsid -> (BlView newDsid, childSources)) <| editDsid dsid
      ]
  in go

emptyBlFromStr : String -> BoundLayout
emptyBlFromStr s = case s of
    "container" -> BlContainer [0]
    "childControl" -> BlChildControl [0] ["hi"]
    "view" -> BlView [0]
    _ -> BlContainer [0]

blStrOpts bl =
  let
    selected  = case bl of
        BlContainer _ -> "container"
        BlChildControl _ _ -> "childControl"
        BlView _ -> "view"
    asOpt s = H.option [HA.value s, HA.selected <| s == selected] [H.text s]
  in List.map asOpt ["container", "childControl", "view"]

editDsid : DataSourceId -> Html DataSourceId
editDsid _ = H.text "dsideditor"

editCsid : ChildSourceId -> Html ChildSourceId
editCsid _ = H.text "csideditor"

editCssid : ChildSourceStateId -> Html ChildSourceStateId
editCssid _ = H.text "cssideditor"

editPattern : Pattern a b -> Html (Pattern a b)
editPattern _ = H.text "not implemented"

listEdit : (a -> Html (a, b)) -> List a -> List (Html (List a, b))
listEdit f =
  let
    go pre l = case l of
        a :: rest ->
            (H.map (\(newA, b) -> (pre ++ (newA :: rest), b)) <| f a) :: go (pre ++ [a]) rest
        [] -> []
  in go []


















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
    goPartial p l = case l of
        LayoutContainer kids -> ClContainer <| Array.map (goPartial p) kids
        LayoutChildChoice q subL -> let ccp = join p q in
            goFull <| expandChildChoice join subL <| Array.map (join ccp) <| getChildPaths <| join ccp q
        LayoutDynamic q -> goPartial (join p q) <| dyn (join p q)
        LayoutLeaf q -> ClLeaf <| join p q
        LayoutSpecial s -> ClSpecial s
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
--             then H.button [HE.onClick <| (lp, EeSubmit LeeRemove)] [H.text "Remove"] :: typeControls
--             else typeControls
--         alwaysControls = [H.button [HE.onClick <| (lp, EeSubmit LeeSubdivide)] [H.text "Split"]]
--       in H.div [] <| alwaysControls ++ layoutControls ++ [body]
--   in go False []
