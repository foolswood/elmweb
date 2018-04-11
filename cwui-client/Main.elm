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
import ClMsgTypes exposing (FromRelayClientBundle, ToRelayClientBundle(..), SubMsg(..), ErrorIndex(..))
import Futility exposing (..)
import PathManipulation exposing (appendSeg)
import Digests exposing (..)
import RemoteState exposing (RemoteState, remoteStateEmpty, NodeMap, TypeMap, TypeAssignMap)
import MonoTime
import Layout exposing (Layout(..), LayoutPath, updateLayout, viewEditLayout, viewLayout, layoutRequires, LayoutEvent)
import Form exposing (FormStore, formStoreEmpty, FormState(..), formState, formUpdate, castFormState)
import TupleViews exposing (viewWithRecent)
import EditTypes exposing (NodeEdit, EditEvent(..), NeChildrenT, mapEe, NeChildState, NodeActions(..), NaChildrenT, NeConstT, constNeConv, childrenNeConv, constNaConv, childrenNaConv)

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
  { errs : List (ErrorIndex, String)
  , viewMode : UiMode
  , bundleCount : Int
  , keepRecent : Float
  -- Layout:
  , layout : Layout Path
  , layoutFs : FormStore LayoutPath Path
  -- Data:
  , recent : List (Digest, RemoteState)
  , subs : Set Path
  , state : RemoteState
  , nodeFs : NodeFs
  , pending : Pending
  }

-- FIXME: Doesn't take edits into account
picksSegs : Maybe (List ContaineeT) -> FormState NeChildrenT -> (NeChildrenT, List Seg)
picksSegs mContainees s =
  let
    containees = Maybe.withDefault [] mContainees
    segs = List.map .seg containees
    defaultPicks = case segs of
        (s :: _) -> Dict.singleton s <| NeChildState True Nothing
        [] -> Dict.empty
    formStateVal = case s of
        FsViewing -> defaultPicks
        FsEditing v -> v
  in (formStateVal, segs)


chosenChildPaths : RemoteState -> NodeFs -> Path -> Array Path
chosenChildPaths rs fs p =
  let
    mChildSegs = case Dict.get p <| .nodes rs of
        Nothing -> Nothing
        Just n -> case n of
            ContainerNode segs -> Just segs
            _ -> Nothing
  in case castFormState (.unwrap childrenNeConv) <| formState p fs of
        Ok neFs ->
          let
            (picks, segs) = picksSegs mChildSegs neFs
            isChosen seg = Maybe.withDefault False <| Maybe.map (.chosen) <| Dict.get seg picks
          in Array.fromList <| List.map (appendSeg p) <| List.filter isChosen segs
        Err _ -> Array.empty

requiredPaths : RemoteState -> NodeFs -> Layout Path -> Set Path
requiredPaths rs fs = layoutRequires (\pa pb -> PathManipulation.canonicalise <| pa ++ pb) (dynamicLayout rs) (chosenChildPaths rs fs)

init : (Model, Cmd Msg)
init =
  let
    initialNodeFs = formStoreEmpty
    initialLayout = LayoutContainer <| Array.fromList [LayoutLeaf "/relay/self", LayoutLeaf "/relay/clients", LayoutChildChoice "/relay/clients" <| LayoutLeaf ""]
    initialState = remoteStateEmpty
    initialSubs = requiredPaths initialState initialNodeFs initialLayout
    initialModel =
      { errs = []
      , viewMode = UmEdit
      , bundleCount = 0
      , keepRecent = 5000.0
      , layout = initialLayout
      , layoutFs = formStoreEmpty
      , recent = []
      , subs = initialSubs
      , state = initialState
      , nodeFs = initialNodeFs
      , pending = Dict.empty
      }
  in (initialModel, subDiffToCmd Set.empty initialSubs)

-- Update

sendBundle : ToRelayClientBundle -> Cmd Msg
sendBundle b = timeStamped (WebSocket.send wsTarget << serialiseBundle b)

subDiffToCmd : Set Path -> Set Path -> Cmd Msg
subDiffToCmd old new =
  let
    added = Set.toList <| Set.diff new old
    removed = Set.toList <| Set.diff old new
    subOps = List.map MsgSub added ++ List.map MsgUnsub removed
  in case subOps of
    [] -> Cmd.none
    _ -> sendBundle (ToRelayClientBundle subOps [] [])

type Msg
  = AddError ErrorIndex String
  | SwapViewMode
  | NetworkEvent FromRelayClientBundle
  | SquashRecent
  | TimeStamped (Time -> Cmd Msg) Time.Time
  | LayoutUiEvent (LayoutPath, EditEvent Path (LayoutEvent Path))
  | NodeUiEvent (Path, EditEvent NodeEdit NodeActions)

timeStamped : (Time -> Cmd Msg) -> Cmd Msg
timeStamped c = Task.perform (TimeStamped c) MonoTime.now

addGlobalError : String -> Model -> (Model, Cmd Msg)
addGlobalError msg m = ({m | errs = (GlobalError, msg) :: .errs m}, Cmd.none)

latestState : Model -> RemoteState
latestState m =
  let
    go recent = case recent of
        [] -> .state m
        ((d, s) :: []) -> s
        (_ :: remainder) -> go remainder
  in go <| .recent m

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    AddError idx msg -> ({model | errs = (idx, msg) :: .errs model}, Cmd.none)
    NetworkEvent b ->
      let
        d = digest b
        (newState, errs) = applyDigest d <| latestState model
        subs = requiredPaths newState (.nodeFs model) (.layout model)
        newM =
          { model
          | errs = errs ++ .errs model
          , subs = subs
          , recent = .recent model ++ [(d, newState)]
          , bundleCount = .bundleCount model + 1
          }
        subCmd = subDiffToCmd (.subs model) subs
        queueSquashCmd = Task.perform (always SquashRecent) <| Process.sleep <| .keepRecent model
      in (newM, Cmd.batch [subCmd, queueSquashCmd])
    SquashRecent ->
        case .recent model of
            ((d, s) :: remaining) -> ({model | state = s, recent = remaining}, Cmd.none)
            [] -> addGlobalError "Tried to squash but no recent" model
    TimeStamped c t -> (model, c (fromFloat t))
    SwapViewMode -> case .viewMode model of
        UmEdit -> ({model | viewMode = UmView}, Cmd.none)
        UmView -> ({model | viewMode = UmEdit}, Cmd.none)
    LayoutUiEvent (p, ue) -> case updateLayout p ue (.layoutFs model) (.layout model) of
        Err msg -> addGlobalError msg model
        Ok (newFs, newLayout) ->
          let
            subs = requiredPaths (latestState model) (.nodeFs model) newLayout
          in ({model | layout = newLayout, layoutFs = newFs, subs = subs}, subDiffToCmd (.subs model) subs)
    NodeUiEvent (p, ue) -> case ue of
        EeUpdate v ->
          let
            newFs = formUpdate p (Just v) <| .nodeFs model
            subs = requiredPaths (latestState model) newFs (.layout model)
          in ({model | nodeFs = newFs, subs = subs}, subDiffToCmd (.subs model) subs)
        EeSubmit na -> case na of
            NaConst wvs ->
                case Dict.get p <| .tyAssns <| latestState model of
                    Nothing -> addGlobalError "Attempting to submit to path of unknown type" model
                    Just (tn, _) -> case Dict.get tn <| .types <| latestState model of
                        Nothing -> addGlobalError "Missing def" model
                        Just def -> case def of
                            TupleDef {types} ->
                              let
                                -- FIXME: Doesn't check anything lines up
                                dum = ClMsgTypes.MsgConstSet
                                  { msgPath = p
                                  , msgTypes = List.map (defWireType << Tuple.second) types
                                  , msgArgs = wvs
                                  , msgAttributee = Nothing
                                  }
                                b = ToRelayClientBundle [] [dum] []
                                newM =
                                  { model
                                  | pending = Dict.insert p na <| .pending model
                                  , nodeFs = formUpdate p Nothing <| .nodeFs model
                                  }
                              in (newM, sendBundle b)
                            _ -> addGlobalError "Def type mismatch" model
            _ -> addGlobalError "Action not implemented" model

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model = WebSocket.listen wsTarget eventFromNetwork

eventFromNetwork : String -> Msg
eventFromNetwork s = case parseBundle s of
    (Ok b) -> NetworkEvent b
    (Err e) -> AddError GlobalError e

-- View

dynamicLayout : RemoteState -> Path -> Layout Path
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
  , case .viewMode m of
    UmEdit -> Html.map LayoutUiEvent <| viewEditLayout "" pathEditView (.layoutFs m) (.layout m)
    UmView -> Html.map NodeUiEvent <| viewLayout
        (++)
        (dynamicLayout <| .state m)
        (chosenChildPaths (.state m) (.nodeFs m))
        (viewPath (.nodeFs m) (.state m) (.recent m) (.pending m))
        (.layout m)
  ]

viewErrors : List (ErrorIndex, String) -> Html a
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

appendMaybe : Maybe a -> List a -> List a
appendMaybe ma l = Maybe.withDefault l <| Maybe.map (\a -> l ++ [a]) ma

viewLoading : Html a
viewLoading = text "Loading..."

viewPath : NodeFs -> RemoteState -> List (Digest, RemoteState) -> Pending -> Path -> Html (Path, EditEvent NodeEdit NodeActions)
viewPath nodeFs baseState recent pending p =
  let
    viewerFor s = case Dict.get p <| .tyAssns s of
        Nothing -> Nothing
        Just (tn, lib) -> Just <| \fs mPending recentCops recentDums -> case Dict.get tn <| .types s of
            Nothing -> text "Missing type information"
            Just def -> viewNode
                lib def (Dict.get p <| .nodes s) recentCops recentDums
                fs mPending
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
   : Liberty -> Definition -> Maybe Node
   -> List Cops -> List DataChange
   -> FormState NodeEdit -> Maybe NodeActions
   -> Html (EditEvent NodeEdit NodeActions)
viewNode lib def maybeNode recentCops recentDums formState maybeNas =
  let
    rcns neConv naConv cn h = viewCasted
        (\(n, s, a) -> Result.map3 (,,) (castMaybe cn n) (castFormState (.unwrap neConv) s) (castMaybe (.unwrap naConv) a))
        (\(mn, fs, mp) -> Html.map (mapEe (.wrap neConv) (.wrap naConv)) <| h mn fs mp)
        (maybeNode, formState, maybeNas)
    editable = lib /= Cannot
  in case def of
    TupleDef d -> case .interpLim d of
        ILUninterpolated -> rcns constNeConv constNaConv (.unwrap constNodeConv) <| viewWithRecent editable d recentDums
        _ -> text "Time series edit not implemented"
    StructDef d -> text "Struct edit not implemented"
    ArrayDef d -> rcns childrenNeConv childrenNaConv (.unwrap childrenNodeConv) <| viewArray d

viewArray
   : ArrayDefinition
  -> Maybe ContainerNodeT -> FormState NeChildrenT -> Maybe NaChildrenT
  -> Html (EditEvent NeChildrenT NaChildrenT)
viewArray arrayDef mn s mp =
  let
    (picks, segs) = picksSegs mn s
    fillChoice seg isPicked mc = case mc of
        Nothing -> Just <| {chosen = isPicked, mod = Nothing}
        Just a -> Just <| {a | chosen = isPicked}
    wrapChoice (seg, isPicked) = EeUpdate <| Dict.update seg (fillChoice seg isPicked) picks
  in Html.map wrapChoice <| viewChildrenChoose segs <| Dict.map (always .chosen) picks

viewChildrenChoose : List Seg -> Dict Seg Bool -> Html (Seg, Bool)
viewChildrenChoose segs chosen =
  let
    selWidget seg = case Maybe.withDefault False <| Dict.get seg chosen of
        True -> Html.li [onClick (seg, False)] [Html.b [] [Html.text seg]]
        False -> Html.li [onClick (seg, True)] [Html.text seg]
  in Html.ol [] <| List.map selWidget segs
