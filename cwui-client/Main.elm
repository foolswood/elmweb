import Array exposing (Array)
import Set exposing (Set)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Time
import Task
import WebSocket
import Process

import Cmp.Set as CSet exposing (CmpSet)
import Cmp.Dict as CDict
import Tagged.Tagged exposing (Tagged(..), tagCmp)
import Tagged.Set as TS exposing (TaggedSet)
import Tagged.Dict as TD
import JsonFudge exposing (serialiseBundle, parseDigest)
import ClTypes exposing (..)
import ClNodes exposing (..)
import ClMsgTypes exposing
  ( ToRelayClientBundle(..), SubMsg(..) , DataErrorIndex(..)
  , ToRelayUpdateBundle(..), ToRelaySubBundle(..))
import Futility exposing (castList, castMaybe, appendMaybe, dictMapMaybe, getWithDefault)
import PathManipulation exposing (appendSeg)
import Digests exposing (Digest, DataChange(..), Cops, seriesChangeCast, constChangeCast, TaOp, applyDigest)
import RemoteState exposing (RemoteState, remoteStateEmpty, NodeMap, TypeMap, TypeAssignMap, remoteStateLookup, requiredPostTypes, ByNs, Valuespace, Postability, allTimeSeries)
import MonoTime
import Layout exposing (BoundLayout(..), ChildSource(..))
import Form exposing (FormStore, formStoreEmpty, FormState(..), formState, formInsert, castFormState, formUpdateEditing)
import TupleViews exposing (viewWithRecent)
import ArrayView exposing (viewArray, defaultChildChoice, remoteChildSegs, arrayActionStateUpdate)
import EditTypes exposing
  ( NodeEdit(NeChildren), EditEvent(..), mapEe, NodeAction(..), NaChildrenT(..)
  , NeConstT, constNeConv, seriesNeConv, childrenNeConv, constNaConv
  , seriesNaConv, childrenNaConv, constPaConv , seriesPaConv, childrenPaConv
  , PendingActions(..), ChildSourceStateId, DataSourceId)
import SequenceOps exposing (SeqOp(..))
import Transience
import TransportTracker exposing (transportSubs, transport, transportCueDum)
import TransportClockView exposing (transportClockView)
import TimeSeriesView exposing (TsModel, tsModelEmpty, TsMsg, viewTimeSeries, TsExternalMsg(..), processTimeSeriesEvent)
import TimeSeries
import DebugInfo

main = Html.program {
    init = init, update = update, subscriptions = subscriptions, view = view}

wsTarget : String
wsTarget = "ws://localhost:8004"

-- Model

type UiMode
  = UmEdit
  | UmView

type alias NodeFs = FormStore Path NodeEdit
type alias NodesFs = ByNs NodeFs

type alias Pending = Dict Path PendingActions
type alias Pendings = ByNs Pending

type alias ChildSelection = CmpSet SubPath (Seg, Path)
type alias ChildSelections = Dict ChildSourceStateId ChildSelection

type alias Model =
  -- Global:
  { errs : List (Namespace, DataErrorIndex, List String)
  , viewMode : UiMode
  , bundleCount : Int
  , keepRecent : Float
  , timeNow : Float
  -- Layout:
  , layout : BoundLayout ChildSourceStateId
  , childSelections : ChildSelections
  -- Special:
  , clockFs : ByNs (FormState EditTypes.PartialTime)
  -- Data:
  , pathSubs : CmpSet SubPath (Seg, Path)
  , postTypeSubs : TaggedSet PostDefinition TypeName
  , recent : List (Digest, RemoteState)
  , state : RemoteState
  , pending : Pendings
  , nodeFs : NodesFs
  }

-- FIXME: Delete later
type SpecialEvent
  = SpeClock Namespace (EditEvent EditTypes.PartialTime Time)

-- FIXME: Just always returns empty
getTsModel : Namespace -> Model -> TsModel
getTsModel ns m = tsModelEmpty
--   let
--     tsm = Maybe.withDefault tsModelEmpty <| CDict.get ns <| .timelines m
--     -- FIXME: Ignores recents and just shows the latest (and reuses path as label):
--     asPointInfo ts = {ts | points = Dict.map
--         (\t tp -> {base = (t, tp), recents = [], fs = FsViewing, mp = Nothing})
--         <| .points ts}
--     asSeriesInfo (p, n, d, e) =
--       { path = p, editable = e, def = d, label = p
--       , transience = Transience.TSteady , series = asPointInfo <| .values n
--       , changedTimes = Dict.empty}
--     rts = List.map asSeriesInfo <| allTimeSeries ns <| latestState m
--   in {tsm | series = rts}

qualifySegs : Path -> Set Seg -> Array Path
qualifySegs p = Array.fromList << List.map (appendSeg p) << Set.toList

subPathDsid : SubPath -> DataSourceId
subPathDsid (Tagged (ns, p)) = "path" :: ns :: String.split "/" (String.dropLeft 1 p)

cementedLayout : RemoteState -> BoundLayout ChildSourceStateId -> ChildSelections -> Layout.ConcreteBoundLayout
cementedLayout rs bl cSels =
  let
    cssidSelected cssid = List.map subPathDsid <| CSet.toList <| getWithDefault TS.empty cssid cSels
  in Layout.cement
    (Layout.resolveChild cssidSelected (dynamicLayout rs))
    cssidSelected
    bl

requiredPaths : RemoteState -> BoundLayout ChildSourceStateId -> ChildSelections -> CmpSet SubPath (Seg, Path)
requiredPaths rs bl cSels =
  let
    layoutRequires = CSet.fromList tagCmp
        <| List.filterMap (Maybe.andThen dspAsPath << Result.toMaybe << dsidToDsp)
        <| Set.toList
        <| Layout.requiredDataSources
        <| cementedLayout rs bl cSels
    -- FIXME: Fixed clock source
    clocksRequire = transportSubs (Tagged "engine") rs
  in CSet.union layoutRequires clocksRequire

init : (Model, Cmd Msg)
init =
  let
    engineNs = Tagged "engine"
    relayNs = Tagged "relay"
    initialNodeFs = TD.empty
    initialLayout = BlContainer <| CsFixed
        [ BlView ["path", "engine", "kinds"] dropCssid
        , BlView ["path", "engine", "types", "amp"] dropCssid
        , BlView ["path", "engine", "types"] ["clients"]
        , BlContainer <| CsTemplate ["clients"] <| BlView [] dropCssid
        , BlView ["clock", "engine"] dropCssid
        , BlSeries ["series"]
        ]
    childSelections = Dict.empty
    initialState = remoteStateEmpty
    initialSubs = requiredPaths initialState initialLayout childSelections
    initialModel =
      { errs = []
      , viewMode = UmEdit
      , bundleCount = 0
      , keepRecent = 5000.0
      , timeNow = 0.0
      , layout = initialLayout
      , childSelections = childSelections
      , clockFs = TD.empty
      , recent = []
      , pathSubs = initialSubs
      , postTypeSubs = TS.empty
      , state = initialState
      , nodeFs = initialNodeFs
      , pending = TD.empty
      }
  in (initialModel, subDiffToCmd (CSet.empty tagCmp) TS.empty initialSubs TS.empty)

-- Update

sendBundle : ToRelayClientBundle -> Cmd Msg
sendBundle b = WebSocket.send wsTarget <| serialiseBundle b <| fromFloat <| MonoTime.rightNow ()

subDiffToCmd
   : CmpSet SubPath (Seg, Path) -> TaggedSet PostDefinition TypeName -> CmpSet SubPath (Seg, Path)
  -> TaggedSet PostDefinition TypeName -> Cmd Msg
subDiffToCmd oldP oldPt newP newPt =
  let
    mapLod a b f = List.map f <| CSet.toList <| CSet.diff a b
    paOps = mapLod newP oldP <| \(Tagged (ns, p)) -> (Tagged ns, MsgSub p)
    prOps = mapLod oldP newP <| \(Tagged (ns, p)) -> (Tagged ns, MsgUnsub p)
    taOps = mapLod newPt oldPt <|
        \(Tagged (ns, ts)) -> (Tagged ns, MsgPostTypeSub <| Tagged ts)
    trOps = mapLod oldPt newPt <|
        \(Tagged (ns, ts)) -> (Tagged ns, MsgPostTypeUnsub <| Tagged ts)
  in case paOps ++ prOps ++ taOps ++ trOps of
    [] -> Cmd.none
    subOps -> sendBundle <| Trcsb <| ToRelaySubBundle subOps

type Msg
  = AddError DataErrorIndex String
  | SwapViewMode
  | NetworkEvent Digest
  | SquashRecent
  | SecondPassedTick
  | LayoutUiEvent (BoundLayout ChildSourceStateId)
  -- FIXME: Get rid of special
  | SpecialUiEvent SpecialEvent
  | TsUiEvent TsMsg
  | NodeUiEvent (SubPath, EditEvent NodeEdit NodeAction)

addDGlobalError : String -> Model -> (Model, Cmd Msg)
addDGlobalError msg m = ({m | errs = (Tagged "UI_INTERNAL", DGlobalError, [msg]) :: .errs m}, Cmd.none)

latestState : Model -> RemoteState
latestState m =
  let
    go recent = case recent of
        [] -> .state m
        ((d, s) :: []) -> s
        (_ :: remainder) -> go remainder
  in go <| .recent m

clearPending : Digest -> Pendings -> Pendings
-- FIXME: Ignores type changes and errors!
clearPending d =
  let
    clearPath ns path pending =
      let
        mNsd = CDict.get ns <| .nsds d
        dops = Maybe.withDefault Dict.empty <| Maybe.map .dops mNsd
        cops = Maybe.withDefault Dict.empty <| Maybe.map .cops mNsd
      in case pending of
        PaChildren pendingCops -> Maybe.map PaChildren <| case Dict.get path cops of
            Nothing -> Just pendingCops
            -- FIXME: This just clears the entire path rather than being more specific
            Just changed -> Nothing
        PaConst wvs -> Maybe.map PaConst <| case Dict.get path dops of
            Nothing -> Just wvs
            Just _ -> Nothing
        PaSeries pendingSeries -> case Dict.get path dops of
            Nothing -> Just <| PaSeries pendingSeries
            Just changes -> case changes of
                TimeChange changedPoints -> Maybe.map PaSeries <| TimeSeries.nonEmpty <|
                    List.foldl TimeSeries.remove pendingSeries <| Dict.keys changedPoints
                _ -> Nothing
  in CDict.map (\ns -> dictMapMaybe <| clearPath ns)

rectifyCop : (Path -> List Seg) -> Path -> Dict Seg (SeqOp Seg) -> NodeFs -> NodeFs
rectifyCop initialState path pathCops fs = formInsert
    path
    (case formState path fs of
        FsEditing (NeChildren es) -> Just <| NeChildren <| ArrayView.rectifyEdits
            (initialState path) pathCops es
        _ -> Nothing
    )
    fs

rectifyEdits : RemoteState -> Digest -> NodesFs -> NodesFs
-- FIXME: Ignores most of the digest
rectifyEdits rs d editFormState =
  let
    rectifyNsEdits ns {cops} fs =
      let
        go nsfs = Dict.foldl
            (rectifyCop (\path
              -> Maybe.withDefault []
              <| Maybe.andThen ((flip remoteChildSegs) path)
              <| CDict.get ns rs))
            nsfs
            <| Dict.map (always <| Dict.map <| always Tuple.second) cops
      in CDict.update ns (Maybe.map go) fs
  in CDict.foldl rectifyNsEdits editFormState <| .nsds d

-- FIXME: Find this a home:
appendSegSp : SubPath -> Seg -> SubPath
appendSegSp (Tagged (ns, p)) s = Tagged (ns, appendSeg p s)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    AddError idx msg -> ({model | errs = (Tagged "UI_INTERNAL", idx, [msg]) :: .errs model}, Cmd.none)
    NetworkEvent d ->
      let
        (newState, errs) = applyDigest d <| latestState model
        pathSubs = requiredPaths newState (.layout model) (.childSelections model)
        postTypeSubs = requiredPostTypes newState
        newM =
          { model
          | errs = (.errs model) ++ (CDict.foldl (\ns es acc -> (List.map (\(idx, msgs) -> (ns, idx, msgs)) es) ++ acc) [] errs)
          , pathSubs = pathSubs
          , postTypeSubs = postTypeSubs
          , recent = .recent model ++ [(d, newState)]
          , bundleCount = .bundleCount model + 1
          , pending = clearPending d <| .pending model
          , nodeFs = rectifyEdits newState d <| .nodeFs model
          }
        subCmd = subDiffToCmd (.pathSubs model) (.postTypeSubs model) pathSubs postTypeSubs
        queueSquashCmd = Task.perform (always SquashRecent) <| Process.sleep <| .keepRecent model
      in (newM, Cmd.batch [subCmd, queueSquashCmd])
    SquashRecent ->
        case .recent model of
            ((d, s) :: remaining) -> ({model | state = s, recent = remaining}, Cmd.none)
            [] -> addDGlobalError "Tried to squash but no recent" model
    SecondPassedTick -> ({model | timeNow = MonoTime.rightNow ()} , Cmd.none)
    SwapViewMode -> case .viewMode model of
        UmEdit -> ({model | viewMode = UmView}, Cmd.none)
        UmView -> ({model | viewMode = UmEdit}, Cmd.none)
    LayoutUiEvent bl ->
          let
            pathSubs = requiredPaths (latestState model) bl (.childSelections model)
          in
            ( {model | layout = bl, pathSubs = pathSubs}
            , subDiffToCmd (.pathSubs model) (.postTypeSubs model) pathSubs (.postTypeSubs model))
    SpecialUiEvent se -> case se of
        SpeClock ns evt -> case evt of
            EeUpdate tp -> ({model | clockFs = CDict.insert ns (FsEditing tp) <| .clockFs model}, Cmd.none)
            EeSubmit t ->
              (model, sendBundle <| Trcub <| ToRelayUpdateBundle ns [transportCueDum t] [])
    TsUiEvent tsMsg -> case processTimeSeriesEvent tsMsg <| getTsModel (Tagged "FIXME") model of
        -- FIXME: Hard coded clock source
        TsemSeek t -> (model, sendBundle <| Trcub <| ToRelayUpdateBundle (Tagged "engine") [transportCueDum t] [])
        -- FIXME: Does nothing
        _ -> (model, Cmd.none)
    NodeUiEvent (sp, ue) ->
      let
        (ns, p) = unSubPath sp
      in case ue of
        EeUpdate v ->
          let
            newFs = CDict.update ns (Maybe.map <| formInsert p (Just v)) <| .nodeFs model
          in ({model | nodeFs = newFs}, Cmd.none)
        EeSubmit na -> case na of
            NaConst wvs -> case remoteStateLookup ns p <| latestState model of
                Err msg -> addDGlobalError ("Error submitting: " ++ msg) model
                Ok (_, def, _, _) -> case def of
                    TupleDef {types} ->
                      let
                        -- FIXME: Doesn't check anything lines up
                        dum = ClMsgTypes.MsgConstSet
                          { msgPath = p
                          , msgTypes = List.map (defWireType << Tuple.second) types
                          , msgArgs = wvs
                          , msgAttributee = Nothing
                          }
                        b = ToRelayUpdateBundle ns [dum] []
                        newM =
                          { model
                          | pending = CDict.update ns (Maybe.map <| Dict.insert p <| PaConst wvs) <| .pending model
                          , nodeFs = CDict.update ns (Maybe.map <| formInsert p Nothing) <| .nodeFs model
                          }
                      in (newM, sendBundle <| Trcub b)
                    _ -> addDGlobalError "Def type mismatch" model
            NaSeries sops -> addDGlobalError "Series submit not implemented" model
            NaChildren nac -> case remoteStateLookup ns p <| latestState model of
                Err msg -> addDGlobalError ("Error submitting: " ++ msg) model
                Ok (_, def, _, postability) ->
                  let
                    updateCmd m = sendBundle <| Trcub <| ToRelayUpdateBundle ns [] [(p, m)]
                  in case def of
                    ArrayDef _ -> case nac of
                        NacMove tgt ref ->
                          ( modifyPendingChildren
                              (Dict.insert tgt (SoPresentAfter ref)) ns p model
                          , updateCmd <| ClMsgTypes.MsgMoveAfter
                              {msgTgt=tgt, msgRef=ref, msgAttributee=Nothing}
                          )
                        NacDelete tgt ->
                          ( modifyPendingChildren
                                (Dict.insert tgt SoAbsent) ns p model
                          , updateCmd <| ClMsgTypes.MsgDelete
                              {msgTgt=tgt, msgAttributee=Nothing}
                          )
                        NacCreate ref postArgs -> case postability of
                            RemoteState.PostableLoaded _ pdef ->
                                ( modifyChildPending
                                    (\p -> {p | creates = CDict.insert phPh (ref, postArgs) <| .creates p})
                                    ns p model
                                , updateCmd <| ClMsgTypes.MsgCreateAfter
                                    {msgTgt=phPh, msgRef = ref, msgAttributee=Nothing, msgPostArgs = List.map2
                                        (\fieldDesc wvs -> Futility.zip (List.map defWireType <| Tuple.second fieldDesc) wvs)
                                        (.fieldDescs pdef) postArgs}
                                )
                            _ -> addDGlobalError "Create on unpostable type" model
                        NacSelect cssid seg -> modifySelection (CSet.insert (appendSegSp sp seg)) cssid model
                        NacDeselect cssid seg -> modifySelection (CSet.remove (appendSegSp sp seg)) cssid model
                    _ -> addDGlobalError "Child modification on non-array" model

modifyChildPending : (EditTypes.PaChildrenT -> EditTypes.PaChildrenT) -> Namespace -> Path -> Model -> Model
modifyChildPending mod ns p model =
  let
    fillBlank mpac = case mpac of
        Just (PaChildren e) -> e
        _ -> {childMods = Dict.empty, creates = CDict.empty tagCmp}
    paUpdate = Just << PaChildren << mod << fillBlank
  in {model | pending = CDict.update
    ns
    (Just << Dict.update p paUpdate << Maybe.withDefault Dict.empty)
    <| .pending model}

modifyPendingChildren : (Dict Seg (SeqOp Seg) -> Dict Seg (SeqOp Seg)) -> Namespace -> Path -> Model -> Model
modifyPendingChildren mod = modifyChildPending (\p -> {p | childMods = mod <| .childMods p})

modifySelection : (CmpSet SubPath (Seg, Path) -> CmpSet SubPath (Seg, Path)) -> ChildSourceStateId -> Model -> (Model, Cmd Msg)
modifySelection mod cssid model =
  let
    newChildSelections = Dict.update
        cssid (Just << mod << Maybe.withDefault (CSet.empty tagCmp))
        <| .childSelections model
    newPathSubs = requiredPaths
        (latestState model) (.layout model) newChildSelections
  in
  ( {model | childSelections = newChildSelections, pathSubs = newPathSubs}
  , subDiffToCmd (.pathSubs model) (.postTypeSubs model) newPathSubs (.postTypeSubs model))

-- FIXME: Using the same placeholder for everything
phPh : Placeholder
phPh = Tagged "ph"

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch
  [ WebSocket.listen wsTarget eventFromNetwork
  , Time.every Time.second <| always SecondPassedTick
  ]

eventFromNetwork : String -> Msg
eventFromNetwork s = case parseDigest s of
    (Ok d) -> NetworkEvent d
    (Err e) -> AddError DGlobalError (e ++ "  <-  " ++ s)

-- View

type DataSourcePtr
  = DsPath SubPath
  | DsClock Namespace

dspAsPath : DataSourcePtr -> Maybe SubPath
dspAsPath dsp = case dsp of
    DsPath sp -> Just sp
    _ -> Nothing

dsidToDsp : DataSourceId -> Result String DataSourcePtr
dsidToDsp dsid = case dsid of
    "path" :: ns :: rest -> Ok <| DsPath <| Tagged (ns, "/" ++ String.join "/" rest)
    ["clock", ns] -> Ok <| DsClock <| Tagged ns
    _ -> Err <| "Invalid dsid: " ++ toString dsid

dropCssid : ChildSourceStateId
dropCssid = ["drop"]

dynamicLayout : RemoteState -> DataSourceId -> ChildSourceStateId -> List (BoundLayout ChildSourceStateId)
dynamicLayout rs dsid seriesCssid = case dsidToDsp dsid of
    Ok (DsPath sp) ->
      let
        (ns, p) = unSubPath sp
      in case remoteStateLookup ns p rs of
        Err _ -> []
        Ok (n, _, _, _) -> case n of
            ContainerNode attributedSegs ->
              let
                cssid = Layout.dataDerivedChildSourceState dsid
                childSource = CsTemplate
                    cssid
                    (BlContainer <| CsDynamic [] cssid)
              in [BlContainer childSource, BlView dsid cssid]
            ConstDataNode _ -> [BlView dsid dropCssid]
            TimeSeriesNode _ -> [BlView dsid seriesCssid]
    Ok (DsClock s) -> []  -- FIXME: This makes sense but is kinda opaque
    Err msg -> []

view : Model -> Html Msg
view m = div []
  [ viewErrors <| .errs m
  , button [onClick SwapViewMode] [text "switcheroo"]
  , text <| "# Bundles: " ++ (toString <| .bundleCount m)
  , DebugInfo.viewRemoteState <| latestState m
  , case .viewMode m of
    UmEdit -> Html.map LayoutUiEvent <| Layout.edit
        (text << toString)
        (.layout m)
    UmView -> Layout.view
        (\dsids ->
          let
            -- FIXME: Hard coded clock source
            rTransp = transport (Tagged "engine") (latestState m) (.timeNow m)
          in case rTransp of
            Ok transp -> Html.map TsUiEvent <| viewTimeSeries
                (getTsModel (Tagged "engine") m) transp
            Err msg -> Html.text <| toString msg)
        (\dsid cssid -> case dsidToDsp dsid of
            Ok (DsPath sp) -> Html.map NodeUiEvent <| viewPath
                (.childSelections m) (.nodeFs m) (.state m) (.recent m)
                (.pending m) cssid sp
            Ok (DsClock s) ->
              let
                transp = transport s (latestState m) (.timeNow m)
              in Html.map (SpecialUiEvent << SpeClock s) <| transportClockView transp <| CDict.getWithDefault FsViewing s <| .clockFs m
            Err msg -> Html.text msg)
        (cementedLayout (latestState m) (.layout m) (.childSelections m))
  ]

viewErrors : List (Namespace, DataErrorIndex, List String) -> Html a
viewErrors errs = ul [] (List.map (\s -> li [] [text <| toString s]) errs)

pathEditView : Maybe SubPath -> FormState SubPath -> Html (EditEvent SubPath SubPath)
pathEditView mp fs = case fs of
    FsViewing -> case mp of
        Nothing -> text "Attempting to view unfilled path"
        Just p -> span [onClick <| EeUpdate p] [text <| toString p]
    FsEditing sp ->
      let
        (partialNs, partialPath) = unSubPath sp
        (Tagged nsSeg) = partialNs
        buttonText = case mp of
            Nothing -> "Set"
            Just p -> "Replace " ++ (toString p)
      in Html.span []
        [ button [onClick <| EeSubmit sp] [text buttonText]
        , input [value nsSeg, type_ "text", onInput <| \pns -> EeUpdate (Tagged (pns, partialPath))] []
        , input [value partialPath, type_ "text", onInput <| EeUpdate << subPath partialNs] []
        ]

viewLoading : String -> Html a
viewLoading s = text <| "Loading " ++ s ++ "..."

viewPath
   : Dict ChildSourceStateId (CmpSet SubPath (Seg, Path)) -> NodesFs -> RemoteState -> List (Digest, RemoteState) -> Pendings
  -> ChildSourceStateId -> SubPath
  -> Html (SubPath, EditEvent NodeEdit NodeAction)
viewPath childSelections nodeFs baseState recent pending cssid sp =
  let
    (ns, p) = unSubPath sp
    viewerFor s = case remoteStateLookup ns p s of
        Err err -> Debug.log ("Lookup failed: " ++ err) Nothing
        Ok (n, def, ed, post) ->
            Just <| \fs mPending recentCops recentDums -> viewNode
                -- FIXME: Awkwardly overcomplicated:
                (\s -> case Dict.get cssid childSelections of
                    Nothing -> False
                    Just dsids -> CSet.member (appendSegSp sp s) dsids)
                ed def post n recentCops recentDums fs mPending cssid
    bordered highlightCol h = div
        [style [("border", "0.2em solid " ++ highlightCol)]] [h]
    viewDigestAfter (d, s) (mPartialViewer, recentCops, recentDums, completeViews, typeChanged) =
      let
        rsGet nsdSub = Maybe.andThen (Dict.get p << nsdSub) <| CDict.get ns <| .nsds d
        newRecentCops = appendMaybe (rsGet .cops) recentCops
        newRecentDums = appendMaybe (rsGet .dops) recentDums
        -- FIXME: Highlight colour thing fairly rubbish, doesn't deactivate controls etc.
        newCompleteView highlightCol partialViewer = bordered highlightCol <|
            partialViewer FsViewing Nothing recentCops recentDums
        newCompleteViews ls = appendMaybe
            (Maybe.map (newCompleteView ls) mPartialViewer) completeViews
      in case rsGet .taOps of
            Nothing -> (mPartialViewer, newRecentCops, newRecentDums, completeViews, typeChanged)
            Just tn -> (viewerFor s, [], [], newCompleteViews "red", True)
    finalise (mPartialViewer, recentCops, recentDums, completeViews, typeChanged) =
      let
        highlight = if typeChanged
          then bordered "green"
          else identity
        finalView = case mPartialViewer of
            Nothing -> if not typeChanged
                then Just <| viewLoading <| toString sp
                else Nothing
            Just partialViewer -> Just <| highlight <| partialViewer
                (formState p <| CDict.getWithDefault formStoreEmpty ns nodeFs)
                (Maybe.andThen (Dict.get p) <| CDict.get ns pending)
                recentCops recentDums
      in appendMaybe finalView completeViews
    contents = finalise <| List.foldl viewDigestAfter (viewerFor baseState, [], [], [], False) recent
  in Html.map (\e -> (subPath ns p, e)) <| div [] contents

viewCasted : (b -> Html r) -> Result String b -> Html r
viewCasted h r = case r of
    Ok b -> h b
    Err m -> text m

viewNode
   : (Seg -> Bool) -> Editable -> Definition -> Postability -> Node -> List (Cops Seg)
  -> List DataChange -> FormState NodeEdit -> Maybe PendingActions -> ChildSourceStateId
  -> Html (EditEvent NodeEdit NodeAction)
viewNode isSelected editable def postability node recentCops recentDums formState maybeNas cssid =
  let
    withCasts recentCast neConv pConv naConv cn recents h = viewCasted
        (\(r, n, fs, mp) -> Html.map (mapEe (.wrap neConv) (.wrap naConv)) <| h r n fs mp)
        (Result.map4 (,,,)
            (castList recentCast recents) (cn node)
            (castFormState (.unwrap neConv) formState) (castMaybe (.unwrap pConv) maybeNas))
  in case def of
    TupleDef d -> case .interpLim d of
        ILUninterpolated -> withCasts
            constChangeCast constNeConv constPaConv constNaConv (.unwrap constNodeConv)
            recentDums (viewWithRecent editable d)
        _ -> withCasts seriesChangeCast seriesNeConv seriesPaConv seriesNaConv (.unwrap seriesNodeConv)
            recentDums (\rs n fs mp -> Html.text <| toString (rs, n, fs, mp))
    StructDef d -> viewStruct d
    ArrayDef d -> withCasts
        Ok childrenNeConv childrenPaConv childrenNaConv
        (.unwrap childrenNodeConv) recentCops
        (\r n fs mp -> viewArray cssid isSelected editable d postability recentCops n fs mp)

viewStruct : StructDefinition -> Html a
viewStruct structDef =
  let
    iw {name} = Html.li [] [Html.text name]
  in Html.ol [] <| List.map iw <| .childDescs structDef
