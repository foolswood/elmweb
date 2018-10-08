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
import HtmlHelpers exposing (listEdit)
import Form exposing (FormState(..), formState, FormStore, formInsert)
import EditTypes exposing (EditEvent(..), mapEe, DataSourceSeg, DataSourceId, ChildSourceStateSeg, ChildSourceStateId)

type BoundLayout dynConfig dsid cssid
  = BlContainer (ChildSource dynConfig dsid cssid)
  | BlView dsid cssid
  -- FIXME: Should have a DataSourceId (or something more strongly typed maybe)
  -- for getting the time series state in the event handler:
  | BlSeries cssid

type ChildSource dynConfig dsid cssid
  = CsFixed (List (BoundLayout dynConfig dsid cssid))
  | CsTemplate
        cssid
        (BoundLayout dynConfig dsid cssid)
  | CsDynamic dsid dynConfig

type ConcreteBoundLayout dsid cssid
  = CblContainer (List (ConcreteBoundLayout dsid cssid))
  | CblView dsid cssid
  | CblSeries (List dsid)

cement
   : (ChildSource dynConfig dsid cssid -> List (BoundLayout dynConfig dsid cssid))
  -> (cssid -> List dsid)
  -> BoundLayout dynConfig dsid cssid -> ConcreteBoundLayout dsid cssid
cement expandContainer stateDataSources l = case l of
    BlContainer childSource -> CblContainer <| List.map
        (cement expandContainer stateDataSources)
        <| expandContainer childSource
    BlView dsid cssid -> CblView dsid cssid
    BlSeries cssid -> CblSeries <| stateDataSources cssid

view
   : (List dsid -> Html a)
  -> (dsid -> cssid -> Html a)
  -> ConcreteBoundLayout dsid cssid -> Html a
view viewSeries viewData =
  let
    go cl = case cl of
        CblContainer subLs -> H.div [] <| List.map go subLs
        CblView dsid cssid -> viewData dsid cssid
        CblSeries dsids -> viewSeries dsids
  in go

expandWildcards : DataSourceId -> ChildSourceStateId -> ChildSourceStateId
expandWildcards dsid =
  let
    expandWildcard seg = if seg == "*"
        then String.join "_" dsid
        else seg
  in List.map expandWildcard

instantiateTemplate
   : (dsid -> dsid -> dsid)
  -> (dsid -> cssid -> cssid)
  -> BoundLayout dynConfig dsid cssid -> dsid
  -> BoundLayout dynConfig dsid cssid
instantiateTemplate joinDsids expander bl dsid = case bl of
    BlContainer cs -> BlContainer <| case cs of
        CsFixed subLs -> CsFixed <| List.map (flip (instantiateTemplate joinDsids expander) dsid) subLs
        CsTemplate subCssid subL -> CsTemplate (expander dsid subCssid) subL
        CsDynamic subDsid a -> CsDynamic (joinDsids dsid subDsid) a
    BlView subDsid subCssid -> BlView
        (joinDsids dsid subDsid) (expander dsid subCssid)
    BlSeries subCssid -> BlSeries <| expander dsid subCssid

resolveChild
   : (ChildSourceStateId -> List DataSourceId)
  -> (DataSourceId -> dynConfig -> List (BoundLayout dynConfig DataSourceId ChildSourceStateId))
  -> ChildSource dynConfig DataSourceId ChildSourceStateId
  -> List (BoundLayout dynConfig DataSourceId ChildSourceStateId)
resolveChild getDsids resolveDynamic childSource =
    case childSource of
        CsFixed subLayouts -> subLayouts
        CsDynamic dsid a -> resolveDynamic dsid a
        CsTemplate cssid subLayout -> List.map
            (instantiateTemplate (++) expandWildcards subLayout)
            (getDsids cssid)

edit
   : (dynConfig -> Html dynConfig)-> BoundLayout dynConfig DataSourceId ChildSourceStateId
  -> Html (BoundLayout dynConfig DataSourceId ChildSourceStateId)
edit dynEdit =
  let
    go bl = H.div []
      [ H.select [HE.onInput emptyBlFromStr] <| blStrOpts bl
      , case bl of
            BlContainer childSource -> H.map BlContainer
              <| case childSource of
                CsFixed subLayouts -> H.map CsFixed
                    <| H.div [] <| listEdit identity go subLayouts
                CsDynamic dsid a -> H.div []
                  [ H.map (\newDsid -> CsDynamic newDsid a) <| editDsid dsid
                  , H.map (\newA -> CsDynamic dsid newA) <| dynEdit a
                  ]
                CsTemplate cssid subL -> H.div []
                  [ H.map
                      (\newCssid -> CsTemplate newCssid subL)
                      <| editCssid cssid
                  , H.map
                      (\newSubL -> CsTemplate cssid newSubL)
                      <| go subL
                  ]
            BlView dsid cssid -> H.div []
              [ H.map (\newDsid -> BlView newDsid cssid) <| editDsid dsid
              , H.map (\newCssid -> BlView dsid newCssid) <| editCssid cssid
              ]
            BlSeries cssid -> H.map (\newCssid -> BlSeries newCssid) <| editCssid cssid
      ]
  in go

emptyBlFromStr : String -> BoundLayout dynConfig DataSourceId ChildSourceStateId
emptyBlFromStr s = case s of
    "container" -> BlContainer <| CsFixed []
    "view" -> BlView [] ["default"]
    "series" -> BlSeries ["series"]
    _ -> BlView ["tosh"] ["tosh"]

blStrOpts bl =
  let
    selected  = case bl of
        BlContainer _ -> "container"
        BlView _ _ -> "view"
        BlSeries _ -> "series"
    asOpt s = H.option [HA.value s, HA.selected <| s == selected] [H.text s]
  in List.map asOpt ["container", "childControl", "view"]

requiredDataSources : ConcreteBoundLayout DataSourceId cssid -> Set DataSourceId
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

editCssid : ChildSourceStateId -> Html ChildSourceStateId
editCssid = editDsid

dataDerivedChildSourceState : DataSourceId -> ChildSourceStateId
dataDerivedChildSourceState dsid = "dataDerived" :: dsid
