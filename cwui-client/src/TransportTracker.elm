module TransportTracker exposing (transport, transportSubs, Transport, TransportLoadError(..), TransportState(..), transportCueDum)

import Dict
import Set exposing (Set)

import ClTypes exposing (Time, Seg, WireValue(..), fromTime, fromFloat, Attributee, Path, WireType(WtTime))
import ClNodes exposing (Node(ConstDataNode), ConstData)
import RemoteState exposing (RemoteState)
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

rConstNode : Path -> RemoteState -> Result TransportLoadError ConstData
rConstNode p rs = case Dict.get p <| .nodes rs of
    Just (ConstDataNode {values}) -> Ok values
    Nothing -> Err NotLoaded
    n -> Err <| BadNodeType <| toString n

ownerClockDiffPath : Seg -> RemoteState -> Result TransportLoadError Path
ownerClockDiffPath ns rs =
  let
    nsOwnerRefPath = appendSeg "/relay/owners" ns
    asRef vs = case vs of
        (ma, [WvString s]) -> Ok (ma, s)
        _ -> Err <| BadWvs <| toString vs
  in Result.map (flip appendSeg "clock_diff" << Tuple.second) <| Result.andThen asRef <| rConstNode nsOwnerRefPath rs

-- FIXME: Doesn't check any types so potential for rubbish errors if anything
-- changes. Also attribution handling is pants.
transport : Seg -> RemoteState -> Float -> Result TransportLoadError Transport
transport ns rs now =
  let
    structPath = asPath [ns, "transport"]
    rSubNode s = rConstNode (appendSeg structPath s) rs
    rTimeDiff = Result.andThen asFloat <| Result.andThen (flip rConstNode rs) <|
        ownerClockDiffPath ns rs
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
        (ma, [WvWord8 i]) -> if i == 0
            then Ok (ma, TransportStopped)
            else if i == 1
                then Ok (ma, TransportRolling)
                else Err <| BadTransportVal i
        _ -> Err <| BadWvs <| toString vs
    playheadPos (_, timeDiff) (_, changedTime) (_, cueTime) = fromFloat <|
        fromTime cueTime + fromTime changedTime + timeDiff + now
    toTransport timeDiff changedTime cueTime (ma, transpState) =
      { state = transpState
      , attributee = ma
      , pos = case transpState of
          TransportStopped -> Tuple.second cueTime
          TransportRolling -> playheadPos timeDiff changedTime cueTime
      }
  in Result.map4 toTransport rTimeDiff rChangedTime rCueTime rTranspState

transportSubs : Seg -> RemoteState -> Set Path
transportSubs ns rs =
  let
    structPath = asPath [ns, "transport"]
    ownerRefPath = appendSeg "/relay/owners" ns
    oipl = case ownerClockDiffPath ns rs of
        Err _ -> []
        Ok p -> [p]
  in Set.fromList <|
    [ appendSeg structPath "state"
    , appendSeg structPath "changed"
    , appendSeg structPath "cue"
    , appendSeg "/relay/owners" ns
    ] ++ oipl

transportCueDum : Seg -> Time -> DataUpdateMsg
transportCueDum ns t = MsgConstSet
  { msgPath = asPath [ns, "transport", "cue"]
  , msgTypes = [WtTime]
  , msgArgs = [WvTime t]
  , msgAttributee = Nothing
  }
