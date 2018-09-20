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
import EditTypes exposing (EditEvent(..), mapEe, DataSourceSeg, DataSourceId, ChildSourceStateSeg, ChildSourceStateId)

type BoundLayout a
  = BlContainer (ChildSource a)
  | BlView DataSourceId ChildSourceStateId
  | BlSeries ChildSourceStateId

type ChildSource a
  = CsFixed (List (BoundLayout a))
  | CsTemplate
        ChildSourceStateId
        (BoundLayout a)
  | CsDynamic DataSourceId a

type ConcreteBoundLayout
  = CblContainer (List ConcreteBoundLayout)
  | CblView DataSourceId ChildSourceStateId
  | CblSeries (List DataSourceId)

cement
   : (ChildSource a -> List (BoundLayout a))
  -> (ChildSourceStateId -> List DataSourceId)
  -> BoundLayout a -> ConcreteBoundLayout
cement expandContainer stateDataSources l = case l of
    BlContainer childSource -> CblContainer <| List.map
        (cement expandContainer stateDataSources)
        <| expandContainer childSource
    BlView dsid cssid -> CblView dsid cssid
    BlSeries cssid -> CblSeries <| stateDataSources cssid

view
   : (List DataSourceId -> Html a)
  -> (DataSourceId -> ChildSourceStateId -> Html a)
  -> ConcreteBoundLayout -> Html a
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

instantiateTemplate : BoundLayout a -> DataSourceId -> BoundLayout a
instantiateTemplate bl dsid = case bl of
    BlContainer cs -> BlContainer <| case cs of
        CsFixed subLs -> CsFixed <| List.map (flip instantiateTemplate dsid) subLs
        CsTemplate subCssid subL -> CsTemplate (expandWildcards dsid subCssid) subL
        CsDynamic subDsid a -> CsDynamic (dsid ++ subDsid) a
    BlView subDsid subCssid -> BlView
        (dsid ++ subDsid) (expandWildcards dsid subCssid)
    BlSeries subCssid -> BlSeries <| expandWildcards dsid subCssid

resolveChild
   : (ChildSourceStateId -> List DataSourceId)
  -> (DataSourceId -> a -> List (BoundLayout a))
  -> ChildSource a -> List (BoundLayout a)
resolveChild getDsids resolveDynamic childSource =
    case childSource of
        CsFixed subLayouts -> subLayouts
        CsDynamic dsid a -> resolveDynamic dsid a
        CsTemplate cssid subLayout -> List.map
            (instantiateTemplate subLayout)
            (getDsids cssid)

lAppend : a -> List a -> List a
lAppend a l = l ++ [a]

edit : (a -> Html a)-> BoundLayout a -> Html (BoundLayout a)
edit dynEdit =
  let
    go : BoundLayout a -> Html (BoundLayout a)
    go bl = H.div []
      [ H.select [HE.onInput emptyBlFromStr] <| blStrOpts bl
      , case bl of
            BlContainer childSource -> H.map BlContainer
              <| case childSource of
                CsFixed subLayouts -> H.map CsFixed
                    <| H.div [] <| listEdit go subLayouts
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

emptyBlFromStr : String -> BoundLayout a
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

listEdit : (a -> Html a) -> List a -> List (Html (List a))
listEdit f =
  let
    go pre l = case l of
        a :: rest ->
            (H.map (\newA -> pre ++ (newA :: rest)) <| f a) :: go (pre ++ [a]) rest
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

editCssid : ChildSourceStateId -> Html ChildSourceStateId
editCssid = editDsid

dataDerivedChildSourceState : DataSourceId -> ChildSourceStateId
dataDerivedChildSourceState dsid = "dataDerived" :: dsid
