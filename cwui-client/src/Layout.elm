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

type alias ChildSourceSeg = String
type alias ChildSourceStateSeg = String
type alias DataSourceSeg = String

type alias ChildSourceId = List ChildSourceSeg
type alias ChildSourceStateId = List ChildSourceStateSeg
type alias DataSourceId = List DataSourceSeg

type BoundLayout
  = BlContainer ChildSourceId
  | BlChildControl DataSourceId ChildSourceStateId
  | BlView DataSourceId
  | BlSeries ChildSourceStateId

type ConcreteBoundLayout
  = CblContainer (List ConcreteBoundLayout)
  | CblChildControl DataSourceId ChildSourceStateId
  | CblView DataSourceId
  | CblSeries (List DataSourceId)

cement
   : (ChildSources a -> ChildSourceId -> (List BoundLayout, ChildSources a))
  -> (ChildSourceStateId -> List DataSourceId)
  -> ChildSources a -> BoundLayout -> ConcreteBoundLayout
cement expandContainer stateDataSources cs l = case l of
    BlContainer csid ->
      let
        (subLs, newChildSources) = expandContainer cs csid
        ncs = Dict.union cs newChildSources
      in CblContainer <|
        List.map (cement expandContainer stateDataSources ncs) subLs
    BlChildControl dsid csid -> CblChildControl dsid csid
    BlView dsid -> CblView dsid
    BlSeries cssid -> CblSeries <| stateDataSources cssid

view
   : (DataSourceId -> Html a) -> (List DataSourceId -> Html a)
  -> (DataSourceId -> ChildSourceStateId -> Html a)
  -> ConcreteBoundLayout -> Html a
view viewData viewSeries viewChildCtl =
  let
    go cl = case cl of
        CblContainer subLs -> H.div [] <| List.map go subLs
        CblChildControl dsid csid -> viewChildCtl dsid csid
        CblView dsid -> viewData dsid
        CblSeries dsids -> viewSeries dsids
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
  | CsDynamic DataSourceId a

type alias ChildSources a = Dict ChildSourceId (ChildSource a)

resolveChild
   : (ChildSourceStateId -> List (ChildSourceSeg, ChildSourceStateSeg, DataSourceSeg))
  -> (DataSourceId -> a -> (List BoundLayout, ChildSources a))
  -> ChildSources a -> ChildSourceId -> (List BoundLayout, ChildSources a)
resolveChild getSegs resolveDynamic childSources csid =
    case getWithDefault (CsFixed []) csid childSources of
        CsFixed subLayouts -> (subLayouts, Dict.empty)
        CsDynamic dsid a -> resolveDynamic dsid a
        CsTemplate cssid csp cssp dsp subLayout ->
          let
            applyPatterns (css, csss, dss) = rebaseBl
                (applyPattern lAppend css csp) (applyPattern lAppend csss cssp)
                (applyPattern lAppend dss dsp) subLayout
          in (List.map applyPatterns <| getSegs cssid, Dict.empty)

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
    BlSeries cssid -> BlSeries <| cssr cssid

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
                    CsDynamic dsid a -> H.div []
                      [ H.map (\newDsid -> (CsDynamic newDsid a, childSources)) <| editDsid dsid
                      , H.map (\newA -> (CsDynamic dsid newA, childSources)) <| dynEdit a
                      ]
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
            BlSeries cssid -> H.map (\newCssid -> (BlSeries newCssid, childSources)) <| editCssid cssid
      ]
  in go

emptyBlFromStr : String -> BoundLayout
emptyBlFromStr s = case s of
    "container" -> BlContainer ["root"]
    "childControl" -> BlChildControl ["root"] ["default"]
    "view" -> BlView ["api", "version"]
    "series" -> BlSeries ["series"]
    _ -> BlContainer ["tosh"]

blStrOpts bl =
  let
    selected  = case bl of
        BlContainer _ -> "container"
        BlChildControl _ _ -> "childControl"
        BlView _ -> "view"
        BlSeries _ -> "series"
    asOpt s = H.option [HA.value s, HA.selected <| s == selected] [H.text s]
  in List.map asOpt ["container", "childControl", "view"]

listEdit : (a -> Html (a, b)) -> List a -> List (Html (List a, b))
listEdit f =
  let
    go pre l = case l of
        a :: rest ->
            (H.map (\(newA, b) -> (pre ++ (newA :: rest), b)) <| f a) :: go (pre ++ [a]) rest
        [] -> []
  in go []

requiredDataSources : ConcreteBoundLayout -> Set DataSourceId
requiredDataSources =
  let
    go cbl acc = case cbl of
        CblContainer subLs -> List.foldl go acc subLs
        CblChildControl dsid _ -> Set.insert dsid acc
        CblView dsid -> Set.insert dsid acc
        CblSeries dsids -> Set.union acc <| Set.fromList dsids
  in flip go Set.empty

-- FIXME: Rest of this is abject tat
editDsid : DataSourceId -> Html DataSourceId
editDsid dsid = H.input
    [HA.value <| String.join "/" dsid, HE.onInput <| String.split "/"]
    []

editCsid : ChildSourceId -> Html ChildSourceId
editCsid = editDsid

editCssid : ChildSourceStateId -> Html ChildSourceStateId
editCssid = editDsid

editPattern : Pattern a b -> Html (Pattern a b)
editPattern _ = H.text "not implemented"

dataDerivedChildSource : DataSourceId -> ChildSourceId
dataDerivedChildSource dsid = "dataDerived" :: dsid

dataDerivedChildSourceState : DataSourceId -> ChildSourceStateId
dataDerivedChildSourceState dsid = "dataDerived" :: dsid
