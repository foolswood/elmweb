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

type BoundLayout dynConfig dsid cssid ssid
  = BlContainer (ChildSource dynConfig dsid cssid ssid)
  | BlView dsid cssid
  | BlSeries ssid cssid

type ChildSource dynConfig dsid cssid ssid
  = CsFixed (List (Maybe String, BoundLayout dynConfig dsid cssid ssid))
  | CsTemplate
        cssid
        (BoundLayout dynConfig dsid cssid ssid)
  | CsDynamic dsid dynConfig

type ConcreteBoundLayout dsid cssid ssid
  = CblContainer (List (Maybe String, ConcreteBoundLayout dsid cssid ssid))
  | CblView dsid cssid
  | CblSeries ssid (List dsid)

cement
   : (ChildSource dynConfig dsid cssid ssid -> List (Maybe String, BoundLayout dynConfig dsid cssid ssid))
  -> (cssid -> List dsid)
  -> BoundLayout dynConfig dsid cssid ssid -> ConcreteBoundLayout dsid cssid ssid
cement expandContainer stateDataSources l = case l of
    BlContainer childSource -> CblContainer <| List.map
        (Tuple.mapSecond <| cement expandContainer stateDataSources)
        <| expandContainer childSource
    BlView dsid cssid -> CblView dsid cssid
    BlSeries ssid cssid -> CblSeries ssid <| stateDataSources cssid

view
   : (ssid -> List dsid -> Html a)
  -> (dsid -> cssid -> Html a)
  -> ConcreteBoundLayout dsid cssid ssid -> Html a
view viewSeries viewData =
  let
    label mLabel = case mLabel of
        Nothing -> identity
        Just label -> \child -> H.div
            [HA.style [("outline", "solid"), ("margin", "0.3em")]]
            [H.small [] [H.text label], child]
    go mLabel cl = label mLabel <| case cl of
        CblContainer subLs -> H.div [] <| List.map (uncurry go) subLs
        CblView dsid cssid -> viewData dsid cssid
        CblSeries ssid dsids -> viewSeries ssid dsids
  in go Nothing

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
  -> BoundLayout dynConfig dsid cssid ssid -> dsid
  -> BoundLayout dynConfig dsid cssid ssid
instantiateTemplate joinDsids expander bl dsid = case bl of
    BlContainer cs -> BlContainer <| case cs of
        CsFixed subLs -> CsFixed <| List.map (Tuple.mapSecond <| flip (instantiateTemplate joinDsids expander) dsid) subLs
        CsTemplate subCssid subL -> CsTemplate (expander dsid subCssid) subL
        CsDynamic subDsid a -> CsDynamic (joinDsids dsid subDsid) a
    BlView subDsid subCssid -> BlView
        (joinDsids dsid subDsid) (expander dsid subCssid)
    BlSeries ssid subCssid -> BlSeries ssid <| expander dsid subCssid

resolveChild
   : (ChildSourceStateId -> List DataSourceId)
  -> (DataSourceId -> dynConfig -> List (BoundLayout dynConfig DataSourceId ChildSourceStateId ssid))
  -> (DataSourceId -> Maybe String)
  -> ChildSource dynConfig DataSourceId ChildSourceStateId ssid
  -> List (Maybe String, BoundLayout dynConfig DataSourceId ChildSourceStateId ssid)
resolveChild getDsids resolveDynamic label childSource =
  let
    unlabelled = List.map <| \l -> (Nothing, l)
  in
    case childSource of
        CsFixed subLayouts -> subLayouts
        CsDynamic dsid a -> unlabelled <| resolveDynamic dsid a
        CsTemplate cssid subLayout -> List.map
            (\dsid -> (label dsid, instantiateTemplate (++) expandWildcards subLayout dsid))
            (getDsids cssid)

edit
   : ssid -> (dynConfig -> Html dynConfig)-> BoundLayout dynConfig DataSourceId ChildSourceStateId ssid
  -> Html (BoundLayout dynConfig DataSourceId ChildSourceStateId ssid)
edit ssid dynEdit =
  let
    go bl = H.div []
      [ H.select [HE.onInput <| emptyBlFromStr ssid] <| blStrOpts bl
      , case bl of
            BlContainer childSource -> H.map BlContainer
              <| case childSource of
                CsFixed subLayouts -> H.map CsFixed
                    <| H.div [HA.style [("outline", "solid"), ("margin", "0.5em")]] <| listEdit
                        identity
                        (\(mL, l) -> H.map (\newL -> (mL, newL)) <| go l)
                        subLayouts
                CsDynamic dsid a -> H.div [HA.style [("outline", "dotted")]]
                  [ H.map (\newDsid -> CsDynamic newDsid a) <| editDsid dsid
                  , H.map (\newA -> CsDynamic dsid newA) <| dynEdit a
                  ]
                CsTemplate cssid subL -> H.div [HA.style [("outline", "dashed"), ("margin", "0.3em")]]
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
            BlSeries ssid cssid -> H.map (\newCssid -> BlSeries ssid newCssid) <| editCssid cssid
      ]
  in go

emptyBlFromStr : ssid -> String -> BoundLayout dynConfig DataSourceId ChildSourceStateId ssid
emptyBlFromStr ssid s = case s of
    "container" -> BlContainer <| CsFixed []
    "view" -> BlView [] ["default"]
    "series" -> BlSeries ssid ["series"]
    _ -> BlView ["tosh"] ["tosh"]

blStrOpts bl =
  let
    selected  = case bl of
        BlContainer _ -> "container"
        BlView _ _ -> "view"
        BlSeries _ _ -> "series"
    asOpt s = H.option [HA.value s, HA.selected <| s == selected] [H.text s]
  in List.map asOpt ["container", "childControl", "view"]

requiredDataSources : ConcreteBoundLayout DataSourceId cssid ssid -> Set DataSourceId
requiredDataSources =
  let
    go cbl acc = case cbl of
        CblContainer subLs -> List.foldl go acc <| List.map Tuple.second subLs
        CblView dsid _ -> Set.insert dsid acc
        CblSeries _ dsids -> Set.union acc <| Set.fromList dsids
  in flip go Set.empty

-- FIXME: Rest of this is abject tat
labelledPathInput : String -> List String -> Html (List String)
labelledPathInput lbl dsid = H.span []
  [ H.small [] [H.text lbl]
  , H.input
    [HA.value <| String.join "/" dsid, HE.onInput <| String.split "/"]
    []
  ]

editDsid : DataSourceId -> Html DataSourceId
editDsid = labelledPathInput "Data:"

editCssid : ChildSourceStateId -> Html ChildSourceStateId
editCssid = labelledPathInput "Child:"

dataDerivedChildSourceState : DataSourceId -> ChildSourceStateId
dataDerivedChildSourceState dsid = "dataDerived" :: dsid
