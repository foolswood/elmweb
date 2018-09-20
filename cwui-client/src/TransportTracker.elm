module TransportTracker exposing (transport, transportSubs, Transport, TransportLoadError(..), TransportState(..), transportCueDum)

import Dict
import Set exposing (Set)

import Cmp.Set as CSet exposing (CmpSet)
import Cmp.Dict as CDict
import Tagged.Tagged exposing (Tagged(..), tagCmp)
import Tagged.Dict as TD
import ClTypes exposing (Time, Namespace, WireValue(..), fromTime, fromFloat, Attributee, Path, WireType(WtTime), SubPath, NsTag, Seg)
import ClNodes exposing (Node(ConstDataNode), ConstData)
import RemoteState exposing (RemoteState, Valuespace)
import PathManipulation exposing (appendSeg, asPath)
import ClMsgTypes exposing (DataUpdateMsg(MsgConstSet))

type TransportState
  = TransportStopped
  | TransportRolling

type alias Transport =
  { pos : Time
  , state : TransportState
  , attributee : Maybe Attributee
  }

type TransportLoadError
  = NotLoaded
  | BadNodeType String
  | BadWvs String
  | BadTransportVal Int

rVs : Namespace -> RemoteState -> Result TransportLoadError Valuespace
rVs ns rs = case CDict.get ns rs of
    Nothing -> Err NotLoaded
    Just vs -> Ok vs

rConstNode : Path -> Valuespace -> Result TransportLoadError ConstData
rConstNode p vs = case Dict.get p <| .nodes vs of
    Just (ConstDataNode {values}) -> Ok values
    Nothing -> Err NotLoaded
    n -> Err <| BadNodeType <| toString n

ownerClockDiffPath : Namespace -> RemoteState -> Result TransportLoadError Path
ownerClockDiffPath (Tagged ns) rs =
  let
    nsOwnerRefPath = appendSeg "/owners" ns
    asRef vs = case vs of
        (ma, [WvString s]) -> Ok (ma, s)
        _ -> Err <| BadWvs <| toString vs
  in
    Result.map (flip appendSeg "clock_diff" << Tuple.second) <|
    Result.andThen asRef <|
    Result.andThen (rConstNode nsOwnerRefPath) <|
    rVs (Tagged "relay") rs

-- FIXME: Doesn't check any types so potential for rubbish errors if anything
-- changes. Also attribution handling is pants.
transport : Namespace -> RemoteState -> Float -> Result TransportLoadError Transport
transport ns rs now =
  let
    structPath = "/transport"
    rSubNode s = Result.andThen (rConstNode (appendSeg structPath s)) <| rVs ns rs
    rTimeDiff =
        Result.andThen asFloat <|
        Result.andThen (uncurry rConstNode) <| Result.map2 (,)
            (ownerClockDiffPath ns rs)
            (rVs ns rs)
    rTranspState = Result.andThen asTranspState <| rSubNode "state"
    rChangedTime = Result.andThen asTime <| rSubNode "changed"
    rCueTime = Result.andThen asTime <| rSubNode "cue"
    asFloat vs = case vs of
        (ma, [WvFloat f]) -> Ok (ma, f)
        _ -> Err <| BadWvs <| toString vs
    asTime vs = case vs of
        (ma, [WvTime t]) -> Ok (ma, t)
        _ -> Err <| BadWvs <| toString vs
    asTranspState vs = case vs of
        (ma, [WvWord32 i]) -> if i == 0
            then Ok (ma, TransportStopped)
            else if i == 1
                then Ok (ma, TransportRolling)
                else Err <| BadTransportVal i
        _ -> Err <| BadWvs <| toString vs
    playheadPos (_, timeDiff) (_, changedTime) (_, cueTime) = fromFloat <|
        fromTime cueTime - fromTime changedTime - timeDiff + now
    toTransport timeDiff changedTime cueTime (ma, transpState) =
      { state = transpState
      , attributee = ma
      , pos = case transpState of
          TransportStopped -> Tuple.second cueTime
          TransportRolling -> playheadPos timeDiff changedTime cueTime
      }
  in Result.map4 toTransport rTimeDiff rChangedTime rCueTime rTranspState

transportSubs : Namespace -> RemoteState -> CmpSet SubPath (Seg, Path)
transportSubs (Tagged ns) rs =
  let
    ownerRefPath = ("relay", appendSeg "/owners" ns)
    oipl = case ownerClockDiffPath (Tagged ns) rs of
        Err _ -> []
        Ok p -> [p]
    structPath = "/transport"
    nsTransp = List.map (\p -> (ns, p)) <|
        [ structPath
        , appendSeg structPath "state"
        , appendSeg structPath "changed"
        , appendSeg structPath "cue"
        ]
  in CSet.fromList tagCmp <| List.map Tagged <| ("relay", "/owners") :: ownerRefPath :: nsTransp

transportCueDum : Time -> DataUpdateMsg
transportCueDum t = MsgConstSet
  { msgPath = "/transport/cue"
  , msgTypes = [WtTime]
  , msgArgs = [WvTime t]
  , msgAttributee = Nothing
  }
