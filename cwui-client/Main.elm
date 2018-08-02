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

import JsonFudge exposing (serialiseBundle, parseBundle)
import ClTypes exposing (..)
import ClNodes exposing (..)
import ClMsgTypes exposing
  ( FromRelayClientBundle, ToRelayClientBundle(..), SubMsg(..)
  , DataErrorIndex(..), ToRelayUpdateBundle(..), ToRelaySubBundle(..))
import Futility exposing (..)
import PathManipulation exposing (appendSeg)
import Digests exposing (..)
import RemoteState exposing (RemoteState, remoteStateEmpty, NodeMap, TypeMap, TypeAssignMap, tyDef)
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

type alias Pending = Dict Path NodeActions

type alias Model =
  -- Global:
  { errs : List (DataErrorIndex, String)
  , viewMode : UiMode
  , bundleCount : Int
  , keepRecent : Float
  , timeNow : Float
  -- Layout:
  , layout : Layout Path Special
  , layoutFs : FormStore LayoutPath Path
  -- Special:
  , clockFs : Dict Seg (FormState EditTypes.PartialTime)
  , timelines : Dict Seg TsModel
  -- Data:
  , recent : List (Digest, RemoteState)
  , pathSubs : Set Path
  , typeSubs : Set TypeName
  , state : RemoteState
  , nodeFs : NodeFs
  , pending : Pending
  }

type Special
  = SpClock Seg
  | SpTimeline Seg

type SpecialEvent
  = SpeClock Seg (EditEvent EditTypes.PartialTime Time)
  | SpeTimeline Seg TsMsg

specialRequire : RemoteState -> Special -> Set Path
specialRequire rs sp = case sp of
    SpClock seg -> transportSubs seg rs
    SpTimeline seg -> transportSubs seg rs

getTsModel : Seg -> Model -> TsModel
getTsModel ns = Maybe.withDefault tsModelEmpty << Dict.get ns << .timelines

qualifySegs : Path -> Set Seg -> Array Path
qualifySegs p = Array.fromList << List.map (appendSeg p) << Set.toList

visibleChildren : RemoteState -> NodeFs -> Path -> Array Path
visibleChildren rs fs p = qualifySegs p <| case chosenChildSegs (formState p fs) of
    Just chosen -> chosen
    Nothing -> defaultChildChoice (remoteChildSegs rs p)

requiredChildren : RemoteState -> NodeFs -> Path -> Array Path
requiredChildren rs fs p = case remoteChildSegs rs p of
    Nothing -> Array.empty
    Just segs ->
      let
        chosen = case chosenChildSegs (formState p fs) of
            Just c -> Set.filter (flip List.member segs) c
            Nothing -> defaultChildChoice <| Just segs
      in qualifySegs p chosen

requiredPaths : RemoteState -> NodeFs -> Layout Path Special -> Set Path
requiredPaths rs fs = layoutRequires
    (\pa pb -> PathManipulation.canonicalise <| pa ++ pb) (dynamicLayout rs)
    (requiredChildren rs fs) (specialRequire rs)

requiredArrayTypes : RemoteState -> Set TypeName
requiredArrayTypes rs =
  let
    eatn _ d = case d of
        ArrayDef {childType, childEditable} -> case childEditable of
            Editable -> Just childType
            _ -> Nothing
        _ -> Nothing
  in Set.fromList <| Dict.values <| dictMapMaybe eatn <| .types rs

init : (Model, Cmd Msg)
init =
  let
    initialNodeFs = formStoreEmpty
    initialLayout = LayoutContainer <| Array.fromList
      [ LayoutSpecial <| SpClock "engine"
      , LayoutSpecial <| SpTimeline "engine"
      , LayoutLeaf "/engine/transport/state"
      , LayoutLeaf "/relay/self"
      , LayoutLeaf "/relay/clients"
      , LayoutChildChoice "/relay/clients" <| LayoutLeaf "/clock_diff"
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
      , typeSubs = Set.empty
      , state = initialState
      , nodeFs = initialNodeFs
      , pending = Dict.empty
      }
  in (initialModel, subDiffToCmd Set.empty Set.empty initialSubs Set.empty)

-- Update

sendBundle : ToRelayClientBundle -> Cmd Msg
sendBundle b = WebSocket.send wsTarget <| serialiseBundle b <| fromFloat <| MonoTime.rightNow ()

subDiffOps : (comparable -> SubMsg) -> (comparable -> SubMsg) -> Set comparable -> Set comparable -> List SubMsg
subDiffOps sub unsub old new =
  let
    added = Set.toList <| Set.diff new old
    removed = Set.toList <| Set.diff old new
  in List.map sub added ++ List.map unsub removed

subDiffToCmd : Set Path -> Set TypeName -> Set Path -> Set TypeName -> Cmd Msg
subDiffToCmd oldP oldT newP newT =
  let
    pOps = subDiffOps MsgSub MsgUnsub oldP newP
    tOps = subDiffOps MsgTypeSub MsgTypeUnsub oldT newT
  in case pOps ++ tOps of
    [] -> Cmd.none
    subOps -> sendBundle <| Trcsb <| ToRelaySubBundle subOps

type Msg
  = AddError DataErrorIndex String
  | SwapViewMode
  | NetworkEvent FromRelayClientBundle
  | SquashRecent
  | SecondPassedTick
  | LayoutUiEvent (LayoutPath, EditEvent Path (LayoutEvent Path Special))
  | SpecialUiEvent SpecialEvent
  | NodeUiEvent (Path, EditEvent NodeEdit NodeActions)

addDGlobalError : String -> Model -> (Model, Cmd Msg)
addDGlobalError msg m = ({m | errs = (DGlobalError, msg) :: .errs m}, Cmd.none)

latestState : Model -> RemoteState
latestState m =
  let
    go recent = case recent of
        [] -> .state m
        ((d, s) :: []) -> s
        (_ :: remainder) -> go remainder
  in go <| .recent m

clearPending : Digest -> Pending -> Pending
-- FIXME: Ignores type changes and errors!
clearPending {dops, cops} =
  let
    clearPath path pending = case pending of
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
  in dictMapMaybe clearPath

rectifyCop : (Path -> List Seg) -> Path -> Dict Seg (SeqOp Seg) -> NodeFs -> NodeFs
rectifyCop initialState path pathCops fs = formInsert
    path
    (case formState path fs of
        FsEditing (NeChildren es) -> Just <| NeChildren <| ArrayView.rectifyEdits
            (initialState path) pathCops es
        _ -> Nothing
    )
    fs

rectifyEdits : RemoteState -> Digest -> NodeFs -> NodeFs
-- FIXME: Ignores most of the digest
rectifyEdits rs {cops} editFormState = Dict.foldl
    (rectifyCop <| \path -> Maybe.withDefault [] <| remoteChildSegs rs path)
    editFormState
    <| Dict.map (always <| Dict.map <| always Tuple.second) cops

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    AddError idx msg -> ({model | errs = (idx, msg) :: .errs model}, Cmd.none)
    NetworkEvent b ->
      let
        d = digest b
        (newState, errs) = applyDigest d <| latestState model
        pathSubs = requiredPaths newState (.nodeFs model) (.layout model)
        typeSubs = requiredArrayTypes newState
        newM =
          { model
          | errs = errs ++ .errs model
          , pathSubs = pathSubs
          , typeSubs = typeSubs
          , recent = .recent model ++ [(d, newState)]
          , bundleCount = .bundleCount model + 1
          , pending = clearPending d <| .pending model
          , nodeFs = rectifyEdits newState d <| .nodeFs model
          }
        subCmd = subDiffToCmd (.pathSubs model) (.typeSubs model) pathSubs typeSubs
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
    LayoutUiEvent (p, ue) -> case updateLayout p ue (.layoutFs model) (.layout model) of
        Err msg -> addDGlobalError msg model
        Ok (newFs, newLayout) ->
          let
            pathSubs = requiredPaths (latestState model) (.nodeFs model) newLayout
          in
            ( {model | layout = newLayout, layoutFs = newFs, pathSubs = pathSubs}
            , subDiffToCmd (.pathSubs model) (.typeSubs model) pathSubs (.typeSubs model))
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
    NodeUiEvent (p, ue) -> case ue of
        EeUpdate v ->
          let
            newFs = formInsert p (Just v) <| .nodeFs model
            pathSubs = requiredPaths (latestState model) newFs (.layout model)
          in
            ( {model | nodeFs = newFs, pathSubs = pathSubs}
            , subDiffToCmd (.pathSubs model) (.typeSubs model) pathSubs (.typeSubs model))
        EeSubmit na -> case na of
            NaConst wvs -> case tyDef p <| latestState model of
                Err msg -> addDGlobalError ("Error submitting: " ++ msg) model
                Ok (def, _) -> case def of
                    TupleDef {types} ->
                      let
                        -- FIXME: Doesn't check anything lines up
                        dum = ClMsgTypes.MsgConstSet
                          { msgPath = p
                          , msgTypes = List.map (defWireType << Tuple.second) types
                          , msgArgs = wvs
                          , msgAttributee = Nothing
                          }
                        b = ToRelayUpdateBundle (dodgyGetNs p) [dum] []
                        newM =
                          { model
                          | pending = Dict.insert p na <| .pending model
                          , nodeFs = formInsert p Nothing <| .nodeFs model
                          }
                      in (newM, sendBundle <| Trcub b)
                    _ -> addDGlobalError "Def type mismatch" model
            NaSeries sops -> addDGlobalError "Series submit not implemented" model
            NaChildren cops -> case tyDef p <| latestState model of
                Err msg -> addDGlobalError ("Error submitting: " ++ msg) model
                Ok (def, _) -> case def of
                    ArrayDef _ ->
                      let
                        mergePending mExisting = Just <| NaChildren <| case mExisting of
                            Just (NaChildren existing) -> Dict.union cops existing
                            _ -> cops
                        newM =
                          { model
                          | pending = Dict.update p mergePending <| .pending model
                          , nodeFs = rectifyCop
                              (Maybe.withDefault [] << remoteChildSegs (latestState model))
                              p cops <| .nodeFs model
                          }
                        b = ToRelayUpdateBundle (dodgyGetNs p) [] <| producePCms p cops
                      in (newM, sendBundle <| Trcub b)
                    _ -> addDGlobalError "Attempted to change children of non-array" model

producePCms : Path -> NaChildrenT -> List ClMsgTypes.ToProviderContainerUpdateMsg
producePCms p =
  let
    -- FIXME: This is complete rubbish (the whole array thing needs rework)
    produceCm (seg, op) = ClMsgTypes.MsgDelete
      {msgPath = "hi", msgTgt = seg, msgAttributee = Nothing}
  in List.map produceCm << Dict.toList

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

dynamicLayout : RemoteState -> Path -> Layout Path a
dynamicLayout rs p = case Dict.get p <| .nodes rs of
    Nothing -> LayoutLeaf p
    Just n -> case n of
        ContainerNode segs -> LayoutContainer <| Array.map (\seg -> dynamicLayout rs (p ++ "/" ++ seg)) <| Array.fromList <| List.map .seg segs
        _ -> LayoutLeaf p

view : Model -> Html Msg
view m = div []
  [ viewErrors <| .errs m
  , button [onClick SwapViewMode] [text "switcheroo"]
  , text <| "# Bundles: " ++ (toString <| .bundleCount m)
  , div [] [text <| toString <| .nodeFs m]
  , case .viewMode m of
    UmEdit -> Html.map LayoutUiEvent <| viewEditLayout "" pathEditView specialEditView (.layoutFs m) (.layout m)
    UmView -> viewLayout
        (++)
        (dynamicLayout <| .state m)
        (visibleChildren (.state m) (.nodeFs m))
        (Html.map NodeUiEvent << viewPath (.nodeFs m) (.state m) (.recent m) (.pending m))
        (Html.map SpecialUiEvent << viewSpecial m)
        (.layout m)
  ]

viewErrors : List (DataErrorIndex, String) -> Html a
viewErrors errs = ul [] (List.map (\s -> li [] [text <| toString s]) errs)

pathEditView : Maybe Path -> FormState Path -> Html (EditEvent Path Path)
pathEditView mp fs = case fs of
    FsViewing -> case mp of
        Nothing -> text "Attempting to view unfilled path"
        Just p -> span [onClick <| EeUpdate p] [text p]
    FsEditing partial ->
      let
        buttonText = case mp of
            Nothing -> "Set"
            Just p -> "Replace " ++ p
      in Html.span []
        [ button [onClick <| EeSubmit partial] [text buttonText]
        , input [value partial, type_ "text", onInput EeUpdate] []
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

viewPath : NodeFs -> RemoteState -> List (Digest, RemoteState) -> Pending -> Path -> Html (Path, EditEvent NodeEdit NodeActions)
viewPath nodeFs baseState recent pending p =
  let
    viewerFor s = case tyDef p s of
        Err _ -> Nothing
        Ok (def, ed) -> Just <| \fs mPending recentCops recentDums -> viewNode
            ed def (Dict.get p <| .nodes s) recentCops recentDums fs mPending
    bordered highlightCol h = div
        [style [("border", "0.2em solid " ++ highlightCol)]] [h]
    viewDigestAfter (d, s) (mPartialViewer, recentCops, recentDums, completeViews, typeChanged) =
      let
        newRecentCops = appendMaybe (Dict.get p <| .cops d) recentCops
        newRecentDums = appendMaybe (Dict.get p <| .dops d) recentDums
        -- FIXME: Highlight colour thing fairly rubbish, doesn't deactivate controls etc.
        newCompleteView highlightCol partialViewer = bordered highlightCol <|
            partialViewer FsViewing Nothing recentCops recentDums
        newCompleteViews ls = appendMaybe
            (Maybe.map (newCompleteView ls) mPartialViewer) completeViews
      in case Dict.get p <| .taOps d of
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
                (formState p nodeFs) (Dict.get p pending) recentCops recentDums
      in appendMaybe finalView completeViews
    contents = finalise <| List.foldl viewDigestAfter (viewerFor baseState, [], [], [], False) recent
  in Html.map (\e -> (p, e)) <| div [] contents

viewCasted : (a -> Result String b) -> (b -> Html r) -> a -> Html r
viewCasted c h a = case c a of
    Ok b -> h b
    Err m -> text m

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

dodgyGetNs : Path -> Namespace
dodgyGetNs p = case String.split "/" p of
    ns :: _ -> ns
    _ -> "dodgyNsFindingDied"
