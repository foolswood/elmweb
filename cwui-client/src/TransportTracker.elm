module TransportTracker exposing (transport, transportSubs, Transport, TransportLoadError(..))

import Dict
import Set exposing (Set)

import ClTypes exposing (Time, Seg, WireValue(..), fromTime, fromFloat, Attributee, Path)
import ClNodes exposing (Node(ConstDataNode), ConstData)
import RemoteState exposing (RemoteState)
import PathManipulation exposing (appendSeg, asPath)

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

ownerInfoPath : Seg -> RemoteState -> Result TransportLoadError Path
ownerInfoPath ns rs =
  let
    nsOwnerRefPath = appendSeg "/relay/owners" ns
    asRef vs = case vs of
        (ma, [WvString s]) -> Ok (ma, s)
        _ -> Err <| BadWvs <| toString vs
  in Result.map Tuple.second <| Result.andThen asRef <| rConstNode nsOwnerRefPath rs

-- FIXME: Doesn't check any types so potential for rubbish errors if anything
-- changes. Also attribution handling is pants.
transport : Seg -> RemoteState -> Float -> Result TransportLoadError Transport
transport ns rs now =
  let
    structPath = asPath [ns, "transport"]
    rSubNode s = rConstNode (appendSeg structPath s) rs
    rTimeDiff = Result.andThen asTime <| Result.andThen (flip rConstNode rs) <|
        ownerInfoPath ns rs
    rTranspState = Result.andThen asTranspState <| rSubNode "state"
    rChangedTime = Result.andThen asTime <| rSubNode "changed"
    rCueTime = Result.andThen asTime <| rSubNode "cue"
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
        fromTime cueTime + fromTime changedTime + fromTime timeDiff - now
    toTransport timeDiff changedTime cueTime (ma, transpState) =
      { state = transpState
      , pos = playheadPos timeDiff changedTime cueTime
      , attributee = ma
      }
  in Result.map4 toTransport rTimeDiff rChangedTime rCueTime rTranspState

transportSubs : Seg -> RemoteState -> Set Path
transportSubs ns rs =
  let
    structPath = asPath [ns, "transport"]
    ownerRefPath = appendSeg "/relay/owners" ns
    oipl = case ownerInfoPath ns rs of
        Err _ -> []
        Ok p -> [p]
  in Set.fromList <|
    [ appendSeg structPath "state"
    , appendSeg structPath "changed"
    , appendSeg structPath "cue"
    , appendSeg "/relay/owners" ns
    ] ++ oipl
