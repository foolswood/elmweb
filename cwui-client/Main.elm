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
import Tagged.Dict as TD exposing (TaggedDict)
import JsonFudge exposing (serialiseBundle, parseBundle)
import ClTypes exposing (..)
import ClNodes exposing (..)
import ClMsgTypes exposing
  ( FromRelayClientBundle, ToRelayClientBundle(..), SubMsg(..)
  , DataErrorIndex(..), ToRelayUpdateBundle(..), ToRelaySubBundle(..))
import Futility exposing (castList, castMaybe, appendMaybe, dictMapMaybe)
import PathManipulation exposing (appendSeg)
import Digests exposing (..)
import RemoteState exposing (RemoteState, remoteStateEmpty, NodeMap, TypeMap, TypeAssignMap, remoteStateLookup, unloadedPostTypes, ByNs, Valuespace, Postability)
import MonoTime
import Layout exposing (Layout(..), LayoutPath, updateLayout, viewEditLayout, viewLayout, layoutRequires, LayoutEvent)
import Form exposing (FormStore, formStoreEmpty, FormState(..), formState, formInsert, castFormState, formUpdateEditing)
import TupleViews exposing (viewWithRecent)
import ArrayView exposing (viewArray, defaultChildChoice, chosenChildSegs, remoteChildSegs, arrayActionStateUpdate)
import EditTypes exposing
  ( NodeEdit(NeChildren), EditEvent(..), mapEe, NodeAction(..), NaChildrenT(..)
  , NeConstT, constNeConv, seriesNeConv, childrenNeConv, constNaConv
  , seriesNaConv, childrenNaConv, constPaConv , seriesPaConv, childrenPaConv
  , PendingActions(..))
import SequenceOps exposing (SeqOp(..))
import TransportTracker exposing (transportSubs, transport, transportCueDum)
import TransportClockView exposing (transportClockView)
import TimeSeriesView exposing (TsModel, tsModelEmpty, TsMsg, viewTimeSeries, TsExternalMsg(..), processTimeSeriesEvent)
import TimeSeries

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

type alias Model =
  -- Global:
  { errs : List (Namespace, DataErrorIndex, String)
  , viewMode : UiMode
  , bundleCount : Int
  , keepRecent : Float
  , timeNow : Float
  -- Layout:
  , layout : Layout SubPath Special
  , layoutFs : FormStore LayoutPath SubPath
  -- Special:
  , clockFs : TaggedDict NsTag String (FormState EditTypes.PartialTime)
  , timelines : TaggedDict NsTag Seg TsModel
  -- Data:
  , recent : List (Digest, RemoteState)
  , pathSubs : CmpSet SubPath (Seg, Path)
  , postTypeSubs : TaggedSet PostDefinition TypeName
  , state : RemoteState
  , nodeFs : NodesFs
  , pending : Pendings
  }

type Special
  = SpClock Namespace
  | SpTimeline Namespace

type SpecialEvent
  = SpeClock Namespace (EditEvent EditTypes.PartialTime Time)
  | SpeTimeline Namespace TsMsg

specialRequire : RemoteState -> Special -> CmpSet SubPath (Seg, Path)
specialRequire rs sp = case sp of
    SpClock ns -> transportSubs ns rs
    SpTimeline ns -> transportSubs ns rs

getTsModel : Namespace -> Model -> TsModel
getTsModel ns = Maybe.withDefault tsModelEmpty << CDict.get ns << .timelines

qualifySegs : Path -> Set Seg -> Array Path
qualifySegs p = Array.fromList << List.map (appendSeg p) << Set.toList

visibleChildren : Valuespace -> NodeFs -> Path -> Array Path
visibleChildren vs fs p = qualifySegs p <| case chosenChildSegs (formState p fs) of
    Just chosen -> chosen
    Nothing -> defaultChildChoice (remoteChildSegs vs p)

requiredChildrenVs : Valuespace -> NodeFs -> Path -> Array Path
requiredChildrenVs vs fs p = case remoteChildSegs vs p of
    Nothing -> Array.empty
    Just segs ->
      let
        chosen = case chosenChildSegs (formState p fs) of
            Just c -> Set.filter (flip List.member segs) c
            Nothing -> defaultChildChoice <| Just segs
      in qualifySegs p chosen

requiredChildren : RemoteState -> NodesFs -> SubPath -> Array SubPath
requiredChildren rs fss sp =
  let
    (ns, p) = unSubPath sp
  in case (CDict.get ns rs, CDict.get ns fss) of
    (Just vs, Just fs) -> Array.map (subPath ns) <| requiredChildrenVs vs fs p
    _ -> Array.empty

requiredPaths : RemoteState -> NodesFs -> Layout SubPath Special -> CmpSet SubPath (Seg, Path)
requiredPaths rs fs = layoutRequires
    tagCmp
    (\(Tagged (ns, pa)) (Tagged (_, pb)) -> Tagged (ns, PathManipulation.canonicalise <| pa ++ pb))
    (dynamicLayout rs)
    (requiredChildren rs fs)
    (specialRequire rs)

init : (Model, Cmd Msg)
init =
  let
    engineNs = Tagged "engine"
    relayNs = Tagged "relay"
    initialNodeFs = TD.empty
    initialLayout = LayoutContainer <| Array.fromList
      [ LayoutSpecial <| SpClock engineNs
      , LayoutSpecial <| SpTimeline engineNs
      , LayoutLeaf <| subPath engineNs "/transport/state"
      , LayoutLeaf <| subPath relayNs "/self"
      , LayoutLeaf <| subPath relayNs "/clients"
      -- FIXME: The leaf of this should be a Path not SubPath:
      , LayoutChildChoice (subPath relayNs "/clients") <| LayoutLeaf <| subPath relayNs "/clock_diff"
      ]
    initialState = remoteStateEmpty
    initialSubs = requiredPaths initialState initialNodeFs initialLayout
    initialModel =
      { errs = []
      , viewMode = UmEdit
      , bundleCount = 0
      , keepRecent = 5000.0
      , timeNow = 0.0
      , layout = initialLayout
      , layoutFs = formStoreEmpty
      , clockFs = TD.empty
      , timelines = TD.empty
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

subDiffOps
   : (a -> SubMsg) -> (a -> SubMsg) -> CmpSet a comparable
  -> CmpSet a comparable -> List SubMsg
subDiffOps sub unsub old new =
  let
    added = CSet.toList <| CSet.diff new old
    removed = CSet.toList <| CSet.diff old new
  in List.map sub added ++ List.map unsub removed

subDiffToCmd
   : CmpSet SubPath (Seg, Path) -> TaggedSet PostDefinition TypeName -> CmpSet SubPath (Seg, Path)
  -> TaggedSet PostDefinition TypeName -> Cmd Msg
subDiffToCmd oldP oldPt newP newPt =
  let
    pOps = subDiffOps MsgSub MsgUnsub oldP newP
    tOps = subDiffOps MsgPostTypeSub MsgPostTypeUnsub oldPt newPt
  in case pOps ++ tOps of
    [] -> Cmd.none
    subOps -> sendBundle <| Trcsb <| ToRelaySubBundle subOps

type Msg
  = AddError DataErrorIndex String
  | SwapViewMode
  | NetworkEvent FromRelayClientBundle
  | SquashRecent
  | SecondPassedTick
  | LayoutUiEvent (LayoutPath, EditEvent SubPath (LayoutEvent SubPath Special))
  | SpecialUiEvent SpecialEvent
  | NodeUiEvent (SubPath, EditEvent NodeEdit NodeAction)

addDGlobalError : String -> Model -> (Model, Cmd Msg)
addDGlobalError msg m = ({m | errs = (Tagged "UI_INTERNAL", DGlobalError, msg) :: .errs m}, Cmd.none)

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

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    AddError idx msg -> ({model | errs = (Tagged "UI_INTERNAL", idx, msg) :: .errs model}, Cmd.none)
    NetworkEvent b ->
      let
        d = digest b
        (newState, errs) = applyDigest d <| latestState model
        pathSubs = requiredPaths newState (.nodeFs model) (.layout model)
        postTypeSubs = unloadedPostTypes newState
        newM =
          { model
          | errs = (.errs model) ++ (CDict.foldl (\ns es acc -> (List.map (\(idx, msg) -> (ns, idx, msg)) es) ++ acc) [] errs)
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
    LayoutUiEvent (sp, ue) -> case updateLayout sp ue (.layoutFs model) (.layout model) of
        Err msg -> addDGlobalError msg model
        Ok (newFs, newLayout) ->
          let
            pathSubs = requiredPaths (latestState model) (.nodeFs model) newLayout
          in
            ( {model | layout = newLayout, layoutFs = newFs, pathSubs = pathSubs}
            , subDiffToCmd (.pathSubs model) (.postTypeSubs model) pathSubs (.postTypeSubs model))
    SpecialUiEvent se -> case se of
        SpeClock ns evt -> case evt of
            EeUpdate tp -> ({model | clockFs = CDict.insert ns (FsEditing tp) <| .clockFs model}, Cmd.none)
            EeSubmit t ->
              (model, sendBundle <| Trcub <| ToRelayUpdateBundle ns [transportCueDum t] [])
        SpeTimeline ns evt -> case processTimeSeriesEvent evt <| getTsModel ns model of
            TsemUpdate tsm -> ({model | timelines = CDict.insert ns tsm <| .timelines model}, Cmd.none)
            TsemSeek t -> (model, sendBundle <| Trcub <| ToRelayUpdateBundle ns [transportCueDum t] [])
            -- FIXME: time point changes go nowhere:
            TsemPointChange _ _ _ -> (model, Cmd.none)
    NodeUiEvent (sp, ue) ->
      let
        (ns, p) = unSubPath sp
      in case ue of
        EeUpdate v ->
          let
            newFs = CDict.update ns (Maybe.map <| formInsert p (Just v)) <| .nodeFs model
            pathSubs = requiredPaths (latestState model) newFs (.layout model)
          in
            ( {model | nodeFs = newFs, pathSubs = pathSubs}
            , subDiffToCmd (.pathSubs model) (.postTypeSubs model) pathSubs (.postTypeSubs model))
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
                Ok (_, def, _, postability) -> case def of
                    ArrayDef _ ->
                      let
                        mergePending mExisting =
                          let
                            existing = case mExisting of
                                Just (PaChildren e) -> e
                                _ -> {childMods = Dict.empty, creates = CDict.empty tagCmp}
                          in Just <| PaChildren <| case nac of
                            NacMove tgt ref -> {existing | childMods = Dict.insert tgt (SoPresentAfter ref) <| .childMods existing}
                            NacDelete tgt -> {existing | childMods = Dict.insert tgt SoAbsent <| .childMods existing}
                            NacCreate mesp wvs -> {existing | creates = CDict.insert phPh (mesp, wvs) <| .creates existing}
                        newM =
                          { model
                          | pending = CDict.update ns (Maybe.map <| Dict.update p mergePending) <| .pending model
                          , nodeFs = CDict.update ns (Maybe.map <| formUpdateEditing p <| arrayActionStateUpdate nac) <| .nodeFs model
                          }
                        b = ToRelayUpdateBundle ns [] <| producePCms postability p nac
                      in (newM, sendBundle <| Trcub b)
                    _ -> addDGlobalError "Attempted to change children of non-array" model

-- FIXME: Using the same placeholder for everything
phPh : Placeholder
phPh = Tagged "ph"

-- FIXME: Shouldn't return a list (or talk about paths)!
producePCms : Postability -> Path -> NaChildrenT -> List (Path, ClMsgTypes.ToProviderContainerUpdateMsg)
producePCms postability p nac = case nac of
    NacCreate mRef wvs -> case postability of
        RemoteState.PostableLoaded pdef -> [(p, ClMsgTypes.MsgCreateAfter
            { msgTgt = phPh, msgRef = mRef, msgAttributee = Nothing
            , msgPostArgs = Futility.zip (List.map (defWireType << Tuple.second) <| .fieldDescs pdef) wvs})]
        _ -> []
    NacMove tgt mRef -> [(p, ClMsgTypes.MsgMoveAfter
        {msgTgt=tgt, msgRef=mRef, msgAttributee=Nothing})]
    NacDelete tgt -> [(p, ClMsgTypes.MsgDelete
        {msgTgt=tgt, msgAttributee=Nothing})]

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch
  [ WebSocket.listen wsTarget eventFromNetwork
  , Time.every Time.second <| always SecondPassedTick
  ]

eventFromNetwork : String -> Msg
eventFromNetwork s = case parseBundle s of
    (Ok b) -> NetworkEvent b
    (Err e) -> AddError DGlobalError e

-- View

dynamicLayout : RemoteState -> SubPath -> Layout SubPath a
dynamicLayout rs sp =
  let
    (ns, p) = unSubPath sp
  in case remoteStateLookup ns p rs of
    Err _ -> LayoutLeaf sp
    Ok (n, _, _, _) -> case n of
        ContainerNode segs -> LayoutContainer <| Array.map (\seg -> dynamicLayout rs (subPath ns <| p ++ "/" ++ seg)) <| Array.fromList <| List.map .seg segs
        _ -> LayoutLeaf sp

view : Model -> Html Msg
view m = div []
  [ viewErrors <| .errs m
  , button [onClick SwapViewMode] [text "switcheroo"]
  , text <| "# Bundles: " ++ (toString <| .bundleCount m)
  , div [] [text <| toString <| .nodeFs m]
  , case .viewMode m of
    UmEdit -> Html.map LayoutUiEvent <| viewEditLayout (subPath (Tagged "") "") pathEditView specialEditView (.layoutFs m) (.layout m)
    UmView -> viewLayout
        -- FIXME: SubPath join shouldn't really be a thing
        (\spa spb ->
          let
            (ns, pa) = unSubPath spa
            (_, pb) = unSubPath spb
          in subPath ns <| pa ++ pb)
        (dynamicLayout <| .state m)
        -- FIXME: This is an utterly minging mess:
        (\sp ->
          let
            (ns, p) = unSubPath sp
          in Array.map (subPath ns) <| Maybe.withDefault Array.empty <| Maybe.map3  visibleChildren (CDict.get ns <| .state m) (CDict.get ns <| .nodeFs m) <| Just p)
        (Html.map NodeUiEvent << viewPath (.nodeFs m) (.state m) (.recent m) (.pending m))
        (Html.map SpecialUiEvent << viewSpecial m)
        (.layout m)
  ]

viewErrors : List (Namespace, DataErrorIndex, String) -> Html a
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

specialEditView : Special -> Html Special
specialEditView sp = text <| toString sp

viewSpecial : Model -> Special -> Html SpecialEvent
viewSpecial m sp = case sp of
    SpClock ns ->
      let
        transp = transport ns (latestState m) (.timeNow m)
      in Html.map (SpeClock ns) <| transportClockView transp <| CDict.getWithDefault FsViewing ns <| .clockFs m
    SpTimeline ns -> case transport ns (latestState m) (.timeNow m) of
        Err e -> Html.text <| toString e
        Ok transp -> Html.map (SpeTimeline ns) <| viewTimeSeries (getTsModel ns m) transp

viewLoading : Html a
viewLoading = text "Loading..."

viewPath
   : NodesFs -> RemoteState -> List (Digest, RemoteState) -> Pendings -> SubPath
  -> Html (SubPath, EditEvent NodeEdit NodeAction)
viewPath nodeFs baseState recent pending sp =
  let
    (ns, p) = unSubPath sp
    viewerFor s = case remoteStateLookup ns p s of
        Err _ -> Nothing
        Ok (n, def, ed, post) ->
            Just <| \fs mPending recentCops recentDums -> viewNode
                ed def post n recentCops recentDums fs mPending
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
            Just OpDemote -> (Nothing, [], [], newCompleteViews "red", True)
            Just (OpAssign tn) -> (viewerFor s, [], [], newCompleteViews "red", True)
    finalise (mPartialViewer, recentCops, recentDums, completeViews, typeChanged) =
      let
        highlight = if typeChanged
          then bordered "green"
          else identity
        finalView = case mPartialViewer of
            Nothing -> if not typeChanged
                then Just <| viewLoading
                else Nothing
            Just partialViewer -> Just <| highlight <| partialViewer
                (formState p <| CDict.getWithDefault formStoreEmpty ns nodeFs)
                (Maybe.andThen (Dict.get p) <| CDict.get ns pending)
                recentCops recentDums
      in appendMaybe finalView completeViews
    contents = finalise <| List.foldl viewDigestAfter (viewerFor baseState, [], [], [], False) recent
  in Html.map (\e -> (subPath ns p, e)) <| div [] contents

viewCasted : (a -> Result String b) -> (b -> Html r) -> a -> Html r
viewCasted c h a = case c a of
    Ok b -> h b
    Err m -> text m

viewNode
   : Editable -> Definition -> Postability -> Node
   -> List Cops -> List DataChange
   -> FormState NodeEdit -> Maybe PendingActions
   -> Html (EditEvent NodeEdit NodeAction)
viewNode editable def postability node recentCops recentDums formState maybeNas =
  let
    withCasts recentCast neConv pConv naConv cn recents h = viewCasted
        (\(r, n, s, a) -> Result.map4 (,,,)
            (castList recentCast r) (cn n)
            (castFormState (.unwrap neConv) s) (castMaybe (.unwrap pConv) a))
        (\(r, n, fs, mp) -> Html.map (mapEe (.wrap neConv) (.wrap naConv)) <| h r n fs mp)
        (recents, node, formState, maybeNas)
  in case def of
    TupleDef d -> case .interpLim d of
        ILUninterpolated -> withCasts
            constChangeCast constNeConv constPaConv constNaConv (.unwrap constNodeConv)
            recentDums (viewWithRecent editable d)
        _ -> withCasts seriesChangeCast seriesNeConv seriesPaConv seriesNaConv (.unwrap seriesNodeConv)
            recentDums (\rs n fs mp -> Html.text <| toString (rs, n, fs, mp))
    StructDef d -> viewStruct d
    ArrayDef d -> withCasts
        Ok childrenNeConv childrenPaConv childrenNaConv (.unwrap childrenNodeConv) recentCops
        (viewArray editable d postability)

viewStruct : StructDefinition -> Html a
viewStruct structDef =
  let
    iw {name} = Html.li [] [Html.text name]
  in Html.ol [] <| List.map iw <| .childDescs structDef
