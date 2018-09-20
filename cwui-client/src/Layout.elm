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
import EditTypes exposing (EditEvent(..), mapEe, ChildSourceSeg, ChildSourceId, DataSourceSeg, DataSourceId, ChildSourceStateSeg, ChildSourceStateId)

type BoundLayout
  = BlContainer ChildSourceId
  | BlView DataSourceId ChildSourceStateId
  | BlSeries ChildSourceStateId

type ConcreteBoundLayout
  = CblContainer (List ConcreteBoundLayout)
  | CblView DataSourceId ChildSourceStateId
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
    BlView dsid csid -> CblView dsid csid
    BlSeries cssid -> CblSeries <| stateDataSources cssid

view
   : (List DataSourceId -> Html a)
  -> (DataSourceId -> ChildSourceStateId -> Html a)
  -> ConcreteBoundLayout -> Html a
view viewSeries viewData =
  let
    go cl = case cl of
        CblContainer subLs -> H.div [] <| List.map go subLs
        CblView dsid csid -> viewData dsid csid
        CblSeries dsids -> viewSeries dsids
  in go

type ChildSource a
  = CsFixed (List BoundLayout)
  | CsTemplate
        ChildSourceStateId
        BoundLayout
  | CsDynamic DataSourceId a

type alias ChildSources a = Dict ChildSourceId (ChildSource a)

resolveChild
   : (ChildSourceStateId -> List DataSourceId)
  -> (DataSourceId -> a -> (List BoundLayout, ChildSources a))
  -> ChildSources a -> ChildSourceId -> (List BoundLayout, ChildSources a)
resolveChild getSegs resolveDynamic childSources csid =
    case getWithDefault (CsFixed []) csid childSources of
        CsFixed subLayouts -> (subLayouts, Dict.empty)
        CsDynamic dsid a -> resolveDynamic dsid a
        -- FIXME: This needs to actually instantiate the template
        CsTemplate cssid subLayout -> ([], Dict.empty)

lAppend : a -> List a -> List a
lAppend a l = l ++ [a]

rebaseBl
   : (ChildSourceId -> ChildSourceId)
  -> (ChildSourceStateId -> ChildSourceStateId)
  -> (DataSourceId -> DataSourceId)
  -> BoundLayout -> BoundLayout
rebaseBl csr cssr dsr bl = case bl of
    BlContainer csid -> BlContainer <| csr csid
    BlView dsid cssid -> BlView (dsr dsid) (cssr cssid)
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
                    CsTemplate cssid subL -> H.div []
                      [ H.map
                          (\newCssid -> (CsTemplate newCssid subL, childSources))
                          <| editCssid cssid
                      , H.map
                          (\(newSubL, newSources) -> (CsTemplate cssid newSubL, newSources))
                          <| go subL
                      ]
              ]
            BlView dsid cssid -> H.div []
              [ H.map (\newDsid -> (BlView newDsid cssid, childSources)) <| editDsid dsid
              , H.map (\newCssid -> (BlView dsid newCssid, childSources)) <| editCssid cssid
              ]
            BlSeries cssid -> H.map (\newCssid -> (BlSeries newCssid, childSources)) <| editCssid cssid
      ]
  in go

emptyBlFromStr : String -> BoundLayout
emptyBlFromStr s = case s of
    "container" -> BlContainer ["root"]
    "view" -> BlView ["root"] ["default"]
    "series" -> BlSeries ["series"]
    _ -> BlContainer ["tosh"]

blStrOpts bl =
  let
    selected  = case bl of
        BlContainer _ -> "container"
        BlView _ _ -> "view"
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
        CblView dsid _ -> Set.insert dsid acc
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

dataDerivedChildSource : DataSourceId -> ChildSourceId
dataDerivedChildSource dsid = "dataDerived" :: dsid

dataDerivedChildSourceState : DataSourceId -> ChildSourceStateId
dataDerivedChildSourceState dsid = "dataDerived" :: dsid
