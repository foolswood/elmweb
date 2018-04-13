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
import RemoteState exposing (RemoteState, remoteStateEmpty, NodeMap, TypeMap, TypeAssignMap, tyDef)
import MonoTime
import Layout exposing (Layout(..), LayoutPath, updateLayout, viewEditLayout, viewLayout, layoutRequires, LayoutEvent)
import Form exposing (FormStore, formStoreEmpty, FormState(..), formState, formUpdate, castFormState)
import TupleViews exposing (viewWithRecent)
import EditTypes exposing (NodeEdit, EditEvent(..), NeChildrenT, mapEe, NeChildState, NodeActions(..), NaChildrenT, NeConstT, constNeConv, childrenNeConv, constNaConv, childrenNaConv)
import SequenceOps exposing (SeqOp(..), applySeqOps)

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
  , pathSubs : Set Path
  , typeSubs : Set TypeName
  , state : RemoteState
  , nodeFs : NodeFs
  , pending : Pending
  }

upstreamSegs : Maybe (List ContaineeT) -> List Seg
upstreamSegs = List.map .seg << Maybe.withDefault []

picksSegs : Maybe (List ContaineeT) -> FormState NeChildrenT -> (NeChildrenT, List Seg)
picksSegs mContainees s =
  let
    uSegs = upstreamSegs mContainees
    segs = case s of
        FsViewing -> uSegs
        FsEditing v -> Result.withDefault ["applySeqOps failed"] <| applySeqOps (dictMapMaybe (always .mod) v) uSegs
    defaultPicks = case segs of
        (s :: _) -> Dict.singleton s <| NeChildState True Nothing
        [] -> Dict.empty
    formStateVal = case s of
        FsViewing -> defaultPicks
        FsEditing v -> v
  in (formStateVal, segs)

chosenChildPaths : Bool -> RemoteState -> NodeFs -> Path -> Array Path
chosenChildPaths remoteOnly rs fs p =
  let
    mChildSegs = case Dict.get p <| .nodes rs of
        Nothing -> Nothing
        Just n -> case n of
            ContainerNode segs -> Just segs
            _ -> Nothing
  in case castFormState (.unwrap childrenNeConv) <| formState p fs of
        Ok neFs ->
          let
            (picks, localSegs) = picksSegs mChildSegs neFs
            segs = if remoteOnly then upstreamSegs mChildSegs else localSegs
            isChosen seg = Maybe.withDefault False <| Maybe.map (.chosen) <| Dict.get seg picks
          in Array.fromList <| List.map (appendSeg p) <| List.filter isChosen segs
        Err _ -> Array.empty

requiredPaths : RemoteState -> NodeFs -> Layout Path -> Set Path
requiredPaths rs fs = layoutRequires
    (\pa pb -> PathManipulation.canonicalise <| pa ++ pb) (dynamicLayout rs)
    (chosenChildPaths True rs fs)

requiredArrayTypes : RemoteState -> Set TypeName
requiredArrayTypes rs =
  let
    eatn _ d = case d of
        ArrayDef {childType, childLiberty} -> case childLiberty of
            Must -> Just childType
            _ -> Nothing
        _ -> Nothing
  in Set.fromList <| Dict.values <| dictMapMaybe eatn <| .types rs

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
      , pathSubs = initialSubs
      , typeSubs = Set.empty
      , state = initialState
      , nodeFs = initialNodeFs
      , pending = Dict.empty
      }
  in (initialModel, subDiffToCmd Set.empty Set.empty initialSubs Set.empty)

-- Update

sendBundle : ToRelayClientBundle -> Cmd Msg
sendBundle b = timeStamped (WebSocket.send wsTarget << serialiseBundle b)

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
    subOps -> sendBundle (ToRelayClientBundle subOps [] [])

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
        pathSubs = requiredPaths newState (.nodeFs model) (.layout model)
        typeSubs = requiredArrayTypes newState
        newM =
          { model
          | errs = errs ++ .errs model
          , pathSubs = pathSubs
          , typeSubs = typeSubs
          , recent = .recent model ++ [(d, newState)]
          , bundleCount = .bundleCount model + 1
          }
        subCmd = subDiffToCmd (.pathSubs model) (.typeSubs model) pathSubs typeSubs
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
            pathSubs = requiredPaths (latestState model) (.nodeFs model) newLayout
          in ({model | layout = newLayout, layoutFs = newFs, pathSubs = pathSubs}, subDiffToCmd (.pathSubs model) (.typeSubs model) pathSubs (.typeSubs model))
    NodeUiEvent (p, ue) -> case ue of
        EeUpdate v ->
          let
            newFs = formUpdate p (Just v) <| .nodeFs model
            pathSubs = requiredPaths (latestState model) newFs (.layout model)
          in ({model | nodeFs = newFs, pathSubs = pathSubs}, subDiffToCmd (.pathSubs model) (.typeSubs model) pathSubs (.typeSubs model))
        EeSubmit na -> case na of
            NaConst wvs ->
                case tyDef p <| latestState model of
                    Err msg -> addGlobalError ("Error submitting: " ++ msg) model
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
  , div [] [text <| toString <| .nodeFs m]
  , case .viewMode m of
    UmEdit -> Html.map LayoutUiEvent <| viewEditLayout "" pathEditView (.layoutFs m) (.layout m)
    UmView -> Html.map NodeUiEvent <| viewLayout
        (++)
        (dynamicLayout <| .state m)
        (chosenChildPaths False (.state m) (.nodeFs m))
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
    viewerFor s = case tyDef p s of
        Err _ -> Nothing
        Ok (def, lib) -> Just <| \fs mPending recentCops recentDums -> viewNode
            lib def (Dict.get p <| .nodes s) recentCops recentDums fs mPending
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
    StructDef d -> viewStruct d
    ArrayDef d -> rcns childrenNeConv childrenNaConv (.unwrap childrenNodeConv) <| viewArray editable d

viewStruct : StructDefinition -> Html a
viewStruct structDef =
  let
    iw {name} = Html.li [] [Html.text name]
  in Html.ol [] <| List.map iw <| .childDescs structDef

viewArray
   : Bool -> ArrayDefinition
  -> Maybe ContainerNodeT -> FormState NeChildrenT -> Maybe NaChildrenT
  -> Html (EditEvent NeChildrenT NaChildrenT)
viewArray editable arrayDef mn s mp =
  let
    (picks, segs) = picksSegs mn s
    fillChoice seg isPicked mc = case mc of
        Nothing -> Just <| {chosen = isPicked, mod = Nothing}
        Just a -> Just <| {a | chosen = isPicked}
    setChoice seg isPicked = EeUpdate <| Dict.update seg (fillChoice seg isPicked) picks
    kidChooser = viewChildrenChoose segs <| Dict.map (always .chosen) picks
    viewChildrenChoose segs chosen =
      let
        selWidget seg = case Maybe.withDefault False <| Dict.get seg chosen of
            True -> Html.span
                [onClick <| setChoice seg False]
                [Html.b [] [Html.text seg]]
            False -> Html.span
                [onClick <| setChoice seg True]
                [Html.text seg]
        itemWidget seg = Html.div [] <| if editable
            then [selWidget seg, addBtn <| Just seg, removeBtn seg]
            else [selWidget seg]
      in Html.div [] <| List.map itemWidget segs
    unusedSeg s = if List.member s segs then unusedSeg <| s ++ "0" else s
    addBtn mPrevSeg =
      let
        newKid = {chosen = True, mod = Just <| SoPresentAfter mPrevSeg}
        s = case mPrevSeg of
            Nothing -> unusedSeg "a"
            Just prevSeg -> unusedSeg <| "post" ++ prevSeg
      in Html.span
        [ onClick <| EeUpdate <| Dict.insert s newKid picks
        , style [("background", "green")]
        ]
        [text "+"]
    removeBtn seg = Html.span
      [ onClick <| EeUpdate <| Dict.insert seg {chosen = False, mod = Just SoAbsent} picks
      , style [("background", "red")]
      ]
      [text "-"]
    content = if editable then [addBtn Nothing, kidChooser] else [kidChooser]
  in Html.div [] content
