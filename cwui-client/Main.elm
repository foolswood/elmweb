import Array exposing (Array)
import Set exposing (Set)
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Time
import Task
import WebSocket
import Process

import Tagged.Set as TS exposing (TaggedSet)
import JsonFudge exposing (serialiseBundle, parseBundle)
import ClTypes exposing (..)
import ClNodes exposing (..)
import ClMsgTypes exposing
  ( FromRelayClientBundle, ToRelayClientBundle(..), SubMsg(..)
  , DataErrorIndex(..), ToRelayUpdateBundle(..), ToRelaySubBundle(..)
  , SubPath)
import Futility exposing (..)
import PathManipulation exposing (appendSeg)
import Digests exposing (..)
import RemoteState exposing (RemoteState, remoteStateEmpty, NodeMap, TypeMap, TypeAssignMap, remoteStateLookup, unloadedPostTypes, ByNs, Valuespace)
import MonoTime
import Layout exposing (Layout(..), LayoutPath, updateLayout, viewEditLayout, viewLayout, layoutRequires, LayoutEvent)
import Form exposing (FormStore, formStoreEmpty, FormState(..), formState, formInsert, castFormState)
import TupleViews exposing (viewWithRecent)
import ArrayView exposing (viewArray, defaultChildChoice, chosenChildSegs, remoteChildSegs)
import EditTypes exposing
  ( NodeEdit(NeChildren), EditEvent(..), mapEe, NodeActions(..), NaChildrenT
  , NeConstT, constNeConv, seriesNeConv, childrenNeConv, constNaConv
  , seriesNaConv, childrenNaConv)
import SequenceOps exposing (SeqOp(..), applySeqOps, banish)
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

type alias Pending = Dict Path NodeActions
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
  , clockFs : Dict Seg (FormState EditTypes.PartialTime)
  , timelines : Dict Seg TsModel
  -- Data:
  , recent : List (Digest, RemoteState)
  , pathSubs : Set SubPath
  , postTypeSubs : TaggedSet PostDefinition TypeName
  , state : RemoteState
  , nodeFs : NodesFs
  , pending : Pendings
  }

type Special
  = SpClock Seg
  | SpTimeline Seg

type SpecialEvent
  = SpeClock Seg (EditEvent EditTypes.PartialTime Time)
  | SpeTimeline Seg TsMsg

specialRequire : RemoteState -> Special -> Set SubPath
specialRequire rs sp = case sp of
    SpClock seg -> transportSubs seg rs
    SpTimeline seg -> transportSubs seg rs

getTsModel : Seg -> Model -> TsModel
getTsModel ns = Maybe.withDefault tsModelEmpty << Dict.get ns << .timelines

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
requiredChildren rs fss (ns, p) = case (Dict.get ns rs, Dict.get ns fss) of
    (Just vs, Just fs) -> Array.map (\p -> (ns, p)) <| requiredChildrenVs vs fs p
    _ -> Array.empty

requiredPaths : RemoteState -> NodesFs -> Layout SubPath Special -> Set SubPath
requiredPaths rs fs = layoutRequires
    (\(ns, pa) (_, pb) -> (ns, PathManipulation.canonicalise <| pa ++ pb))
    (dynamicLayout rs)
    (requiredChildren rs fs)
    (specialRequire rs)

init : (Model, Cmd Msg)
init =
  let
    initialNodeFs = Dict.empty
    initialLayout = LayoutContainer <| Array.fromList
      [ LayoutSpecial <| SpClock "engine"
      , LayoutSpecial <| SpTimeline "engine"
      , LayoutLeaf ("engine", "/transport/state")
      , LayoutLeaf ("relay", "/self")
      , LayoutLeaf ("relay", "/clients")
      -- FIXME: The leaf of this should be a Path not SubPath:
      , LayoutChildChoice ("relay", "/clients") <| LayoutLeaf ("relay", "/clock_diff")
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
      , clockFs = Dict.empty
      , timelines = Dict.empty
      , recent = []
      , pathSubs = initialSubs
      , postTypeSubs = TS.empty
      , state = initialState
      , nodeFs = initialNodeFs
      , pending = Dict.empty
      }
  in (initialModel, subDiffToCmd Set.empty TS.empty initialSubs TS.empty)

-- Update

sendBundle : ToRelayClientBundle -> Cmd Msg
sendBundle b = WebSocket.send wsTarget <| serialiseBundle b <| fromFloat <| MonoTime.rightNow ()

subDiffOps
   : (a -> List b) -> (a -> a -> a) -> (b -> SubMsg)
  -> (b -> SubMsg) -> a -> a -> List SubMsg
subDiffOps toList diff sub unsub old new =
  let
    added = toList <| diff new old
    removed = toList <| diff old new
  in List.map sub added ++ List.map unsub removed

subDiffToCmd
   : Set SubPath -> TaggedSet PostDefinition TypeName -> Set SubPath
  -> TaggedSet PostDefinition TypeName -> Cmd Msg
subDiffToCmd oldP oldPt newP newPt =
  let
    pOps = subDiffOps Set.toList Set.diff MsgSub MsgUnsub oldP newP
    tOps = subDiffOps TS.toList TS.diff MsgPostTypeSub MsgPostTypeUnsub oldPt newPt
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
  | NodeUiEvent (SubPath, EditEvent NodeEdit NodeActions)

addDGlobalError : String -> Model -> (Model, Cmd Msg)
addDGlobalError msg m = ({m | errs = ("UI_INTERNAL", DGlobalError, msg) :: .errs m}, Cmd.none)

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
        mNsd = Dict.get ns <| .nsds d
        dops = Maybe.withDefault Dict.empty <| Maybe.map .dops mNsd
        cops = Maybe.withDefault Dict.empty <| Maybe.map .cops mNsd
      in case pending of
        NaChildren pendingCops -> Maybe.map NaChildren <| case Dict.get path cops of
            Nothing -> Just pendingCops
            Just changed -> nonEmptyDict <| removeKeys (Dict.keys changed) pendingCops
        NaConst wvs -> Maybe.map NaConst <| case Dict.get path dops of
            Nothing -> Just wvs
            Just _ -> Nothing
        NaSeries pendingSeries -> case Dict.get path dops of
            Nothing -> Just <| NaSeries pendingSeries
            Just changes -> case changes of
                TimeChange changedPoints -> Maybe.map NaSeries <| TimeSeries.nonEmpty <|
                    List.foldl TimeSeries.remove pendingSeries <| Dict.keys changedPoints
                _ -> Nothing
  in Dict.map (\ns -> dictMapMaybe <| clearPath ns)

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
              <| Dict.get ns rs))
            nsfs
            <| Dict.map (always <| Dict.map <| always Tuple.second) cops
      in Dict.update ns (Maybe.map go) fs
  in Dict.foldl rectifyNsEdits editFormState <| .nsds d

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    AddError idx msg -> ({model | errs = ("UI_INTERNAL", idx, msg) :: .errs model}, Cmd.none)
    NetworkEvent b ->
      let
        d = digest b
        (newState, errs) = applyDigest d <| latestState model
        pathSubs = requiredPaths newState (.nodeFs model) (.layout model)
        postTypeSubs = unloadedPostTypes newState
        newM =
          { model
          | errs = (.errs model) ++ (Dict.foldl (\ns es acc -> (List.map (\(idx, msg) -> (ns, idx, msg)) es) ++ acc) [] errs)
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
        SpeClock seg evt -> case evt of
            EeUpdate tp -> ({model | clockFs = Dict.insert seg (FsEditing tp) <| .clockFs model}, Cmd.none)
            EeSubmit t ->
              (model, sendBundle <| Trcub <| ToRelayUpdateBundle seg [transportCueDum seg t] [])
        SpeTimeline ns evt -> case processTimeSeriesEvent evt <| getTsModel ns model of
            TsemUpdate tsm -> ({model | timelines = Dict.insert ns tsm <| .timelines model}, Cmd.none)
            TsemSeek t -> (model, sendBundle <| Trcub <| ToRelayUpdateBundle ns [transportCueDum ns t] [])
            -- FIXME: time point changes go nowhere:
            TsemPointChange _ _ _ -> (model, Cmd.none)
    NodeUiEvent ((ns, p), ue) -> case ue of
        EeUpdate v ->
          let
            newFs = Dict.update ns (Maybe.map <| formInsert p (Just v)) <| .nodeFs model
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
                          | pending = Dict.update ns (Maybe.map <| Dict.insert p na) <| .pending model
                          , nodeFs = Dict.update ns (Maybe.map <| formInsert p Nothing) <| .nodeFs model
                          }
                      in (newM, sendBundle <| Trcub b)
                    _ -> addDGlobalError "Def type mismatch" model
            NaSeries sops -> addDGlobalError "Series submit not implemented" model
            NaChildren cops -> case remoteStateLookup ns p <| latestState model of
                Err msg -> addDGlobalError ("Error submitting: " ++ msg) model
                Ok (_, def, _, _) -> case def of
                    ArrayDef _ ->
                      let
                        mergePending mExisting = Just <| NaChildren <| case mExisting of
                            Just (NaChildren existing) -> Dict.union cops existing
                            _ -> cops
                        newM =
                          { model
                          | pending = Dict.update ns (Maybe.map <| Dict.update p mergePending) <| .pending model
                          , nodeFs = Dict.update ns (Maybe.map <| rectifyCop
                              (Maybe.withDefault [] << remoteChildSegs (getWithDefault RemoteState.vsEmpty ns <| latestState model))
                              p cops) <| .nodeFs model
                          }
                        b = ToRelayUpdateBundle ns [] <| producePCms p cops
                      in (newM, sendBundle <| Trcub b)
                    _ -> addDGlobalError "Attempted to change children of non-array" model

producePCms : Path -> NaChildrenT -> List (Namespace, ClMsgTypes.ToProviderContainerUpdateMsg)
producePCms p _ = [] -- FIXME: complete non functional junk

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
dynamicLayout rs (ns, p) = case remoteStateLookup ns p rs of
    Err _ -> LayoutLeaf (ns, p)
    Ok (n, _, _, _) -> case n of
        ContainerNode segs -> LayoutContainer <| Array.map (\seg -> dynamicLayout rs (ns, p ++ "/" ++ seg)) <| Array.fromList <| List.map .seg segs
        _ -> LayoutLeaf (ns, p)

view : Model -> Html Msg
view m = div []
  [ viewErrors <| .errs m
  , button [onClick SwapViewMode] [text "switcheroo"]
  , text <| "# Bundles: " ++ (toString <| .bundleCount m)
  , div [] [text <| toString <| .nodeFs m]
  , case .viewMode m of
    UmEdit -> Html.map LayoutUiEvent <| viewEditLayout ("", "") pathEditView specialEditView (.layoutFs m) (.layout m)
    UmView -> viewLayout
        -- FIXME: SubPath join shouldn't really be a thing
        (\(ns, pa) (_, pb) -> (ns, pa ++ pb))
        (dynamicLayout <| .state m)
        -- FIXME: This is an utterly minging mess:
        (\(ns, p) -> Array.map (\p -> (ns, p)) <| Maybe.withDefault Array.empty <| Maybe.map3  visibleChildren (Dict.get ns <| .state m) (Dict.get ns <| .nodeFs m) <| Just p)
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
    FsEditing (partialNs, partialPath) ->
      let
        buttonText = case mp of
            Nothing -> "Set"
            Just p -> "Replace " ++ (toString p)
      in Html.span []
        [ button [onClick <| EeSubmit (partialNs, partialPath)] [text buttonText]
        , input [value partialNs, type_ "text", onInput <| \pns -> EeUpdate (pns, partialPath)] []
        , input [value partialPath, type_ "text", onInput <| \pp -> EeUpdate (partialNs, pp)] []
        ]

specialEditView : Special -> Html Special
specialEditView sp = text <| toString sp

viewSpecial : Model -> Special -> Html SpecialEvent
viewSpecial m sp = case sp of
    SpClock seg ->
      let
        transp = transport seg (latestState m) (.timeNow m)
      in Html.map (SpeClock seg) <| transportClockView transp <| formState seg <| .clockFs m
    SpTimeline seg -> case transport seg (latestState m) (.timeNow m) of
        Err e -> Html.text <| toString e
        Ok transp -> Html.map (SpeTimeline seg) <| viewTimeSeries (getTsModel seg m) transp

viewLoading : Html a
viewLoading = text "Loading..."

viewPath
   : NodesFs -> RemoteState -> List (Digest, RemoteState) -> Pendings -> SubPath
  -> Html (SubPath, EditEvent NodeEdit NodeActions)
viewPath nodeFs baseState recent pending (ns, p) =
  let
    viewerFor s = case remoteStateLookup ns p s of
        Err _ -> Nothing
        Ok (n, def, ed, post) ->
            Just <| \fs mPending recentCops recentDums -> viewNode
                ed def (Just n) recentCops recentDums fs mPending
    bordered highlightCol h = div
        [style [("border", "0.2em solid " ++ highlightCol)]] [h]
    viewDigestAfter (d, s) (mPartialViewer, recentCops, recentDums, completeViews, typeChanged) =
      let
        rsGet nsdSub = Maybe.andThen (Dict.get p << nsdSub) <| Dict.get ns <| .nsds d
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
                (formState p <| getWithDefault formStoreEmpty ns nodeFs)
                (Maybe.andThen (Dict.get p) <| Dict.get ns pending)
                recentCops recentDums
      in appendMaybe finalView completeViews
    contents = finalise <| List.foldl viewDigestAfter (viewerFor baseState, [], [], [], False) recent
  in Html.map (\e -> ((ns, p), e)) <| div [] contents

viewCasted : (a -> Result String b) -> (b -> Html r) -> a -> Html r
viewCasted c h a = case c a of
    Ok b -> h b
    Err m -> text m

-- FIXME: Shouldn't be Node not Maybe Node
viewNode
   : Editable -> Definition -> Maybe Node
   -> List Cops -> List DataChange
   -> FormState NodeEdit -> Maybe NodeActions
   -> Html (EditEvent NodeEdit NodeActions)
viewNode ed def maybeNode recentCops recentDums formState maybeNas =
  let
    withCasts recentCast neConv naConv cn recents h = viewCasted
        (\(r, n, s, a) -> Result.map4 (,,,)
            (castList recentCast r) (castMaybe cn n)
            (castFormState (.unwrap neConv) s) (castMaybe (.unwrap naConv) a))
        (\(r, mn, fs, mp) -> Html.map (mapEe (.wrap neConv) (.wrap naConv)) <| h r mn fs mp)
        (recents, maybeNode, formState, maybeNas)
    editable = ed /= ReadOnly -- FIXME: Plumb this type all the way through
  in case def of
    TupleDef d -> case .interpLim d of
        ILUninterpolated -> withCasts
            constChangeCast constNeConv constNaConv (.unwrap constNodeConv)
            recentDums (viewWithRecent editable d)
        _ -> withCasts seriesChangeCast seriesNeConv seriesNaConv (.unwrap seriesNodeConv)
            recentDums (\rs mn fs mp -> Html.text <| toString (rs, mn, fs, mp))
    StructDef d -> viewStruct d
    ArrayDef d -> withCasts
        Ok childrenNeConv childrenNaConv (.unwrap childrenNodeConv) recentCops
        (viewArray editable d)

viewStruct : StructDefinition -> Html a
viewStruct structDef =
  let
    iw {name} = Html.li [] [Html.text name]
  in Html.ol [] <| List.map iw <| .childDescs structDef
