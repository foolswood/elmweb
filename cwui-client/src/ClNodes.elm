module ClNodes exposing (..)

import Dict exposing (Dict)

import ClTypes exposing (Seg, Attributee, WireValue, WireType, TpId, Time, Interpolation)

type alias ConstData = (Maybe Attributee, List WireValue)

type alias TimePoint =
  { time : Time
  , attributee : Maybe Attributee
  , wvs : List WireValue
  , interpolation : Interpolation
  , errors : List String
  }
type alias TimeSeries a = Dict TpId a

type alias ConstDataNodeT =
  { types : List WireType
  , values : ConstData
  , pending : Maybe ConstData
  }

type alias TimeSeriesNodeT =
  { types : List WireType
  , values : TimeSeries TimePoint
  , pending : TimeSeries TimePoint
  , removed : Dict TpId (Maybe Attributee)
  }

-- FIXME: This should have pending stuff, or it should be elsewhere?
type alias ContainerNodeT =
  { children : List Seg
  }

type NodeT
  = ConstDataNode ConstDataNodeT
  | TimeSeriesNode TimeSeriesNodeT
  | ContainerNode ContainerNodeT
  | UnpopulatedNode

type alias Node =
  { errors : List String
  , body : NodeT
  }

unpopulatedNode : Node
unpopulatedNode = Node [] UnpopulatedNode

setConstData : List WireType -> ConstData -> Node -> Node
setConstData wts cd n =
  let
    fromEmptyWithErr msg = setConstData
        wts cd {unpopulatedNode | errors = msg :: .errors n}
    newBody = {n | body = ConstDataNode
      { types = wts
      , values = cd
      , pending = Nothing
      }}
  in case .body n of
    UnpopulatedNode -> newBody
    ConstDataNode cdn ->
      if .types cdn == wts then
        newBody
      else
        fromEmptyWithErr "Const node unexpectedly changed wire type"
    _ -> fromEmptyWithErr "Const set on non-const node, dropped existing data"

setTimePoint
  :  List WireType -> TpId -> Time -> Maybe Attributee -> List WireValue
  -> Interpolation -> Node -> Node
setTimePoint wts tpid t ma wvs i n =
  let
    fromEmptyWithErr msg = setTimePoint
        wts tpid t ma wvs i {unpopulatedNode | errors = msg :: .errors n}
    tp errs = 
      { time = t
      , attributee = ma
      , wvs = wvs
      , interpolation = i
      , errors = errs
      }
    replaceVal motp = Just <| case motp of
        Nothing -> tp []
        Just otp -> tp <| .errors otp
  in case .body n of
    UnpopulatedNode ->
      {n | body = TimeSeriesNode
        { types = wts
        , values = Dict.singleton tpid <| tp []
        , pending = Dict.empty
        , removed = Dict.empty
        }
      }
    TimeSeriesNode tsn ->
      if .types tsn == wts then
        { errors = .errors n
        , body = TimeSeriesNode
            {tsn | values = Dict.update tpid replaceVal <| .values tsn}
        }
      else
        fromEmptyWithErr "Adding timepoint of different WireType dropped previous series"
    _ -> fromEmptyWithErr "Node type change dropped existing data"

removeTimePoint : TpId -> Maybe Attributee -> Node -> Node
removeTimePoint tpid ma n = case .body n of
    TimeSeriesNode tsn -> {n | body = TimeSeriesNode {tsn
      | values = Dict.remove tpid <| .values tsn
      , removed = Dict.insert tpid ma <| .removed tsn
      }}
    _ -> {n | errors = "Attempted to remove timepoint from non-series" :: .errors n}

-- FIXME: The attributee is currently dropped in the child* functions

childPresentAfter : Maybe Attributee -> Seg -> Maybe Seg -> Node -> Node
childPresentAfter att tgt mRef n =
  let
    fromEmptyWithErr msg = {n
      | body = ContainerNode {children = [tgt]}, errors = msg :: .errors n}
    insertAfter ref acc remaining = case remaining of
        (v :: leftover) -> if v == ref
            then Ok <| acc ++ (tgt :: remaining)
            else insertAfter ref (acc ++ [v]) leftover
        [] -> Err <| "Ref not present: " ++ ref
    tgtRemoved = List.filter ((/=) tgt)
    presentAfter existing = case mRef of
        Nothing -> Ok <| tgt :: tgtRemoved existing
        Just ref -> insertAfter ref [] existing
    nodeWithKids kids = {n | body = ContainerNode {children = kids}}
  in
    case .body n of
        UnpopulatedNode -> case presentAfter [] of
            Ok kids -> nodeWithKids kids
            Err msg -> fromEmptyWithErr msg
        ContainerNode {children} -> case presentAfter children of
            Ok kids -> nodeWithKids kids
            Err msg -> {n
              | body = ContainerNode {children = tgt :: tgtRemoved children}
              , errors = msg :: .errors n}
        _ -> fromEmptyWithErr "Child present applied to non-container type"

childAbsent : Maybe Attributee -> Seg -> Node -> Node
childAbsent att tgt n = case .body n of
    ContainerNode {children} ->
        {n | body = ContainerNode {children = List.filter ((/=) tgt) children}}
    _ -> {n | errors = "Attempted to remove child from non-container" :: .errors n}
