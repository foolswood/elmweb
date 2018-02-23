module ClMsgTypes exposing (..)
import ClTypes exposing (Path, Seg, TypeName, Attributee, TpId, Time, Interpolation, Definition, Liberty, WireValue, WireType)

type SubMsg
  = MsgSub Path
  | MsgTypeSub TypeName
  | MsgUnsub Path
  | MsgTypeUnsub TypeName

-- Note: unlike the haskell one this is always TypeName
type ErrorIndex
  = GlobalError
  | PathError Path
  | TimePointError Path TpId
  -- FIXME: Is it impossible for a client to trigger this:
  | TypeError TypeName

type MsgError = MsgError ErrorIndex String

type DataUpdateMsg
  = MsgConstSet
      { msgPath : Path
      , msgTypes : (List WireType)
      , msgArgs : (List WireValue)
      , msgAttributee : (Maybe Attributee)
      }
  | MsgSet
      { msgPath : Path
      , msgTpId : TpId
      , msgTime : Time
      , msgTypes : (List WireType)
      , msgArgs : (List WireValue)
      , msgInterpolation : Interpolation
      , msgAttributee : (Maybe Attributee)
      }
  | MsgRemove
      { msgPath : Path
      , msgTpId : TpId
      , msgAttributee : (Maybe Attributee)
      }

dumPath : DataUpdateMsg -> Path
dumPath dum = case dum of
    MsgConstSet {msgPath} -> msgPath
    MsgSet {msgPath} -> msgPath
    MsgRemove {msgPath} -> msgPath

type ContainerUpdateMsg
  = MsgPresentAfter
      { msgPath : Path
      , msgTgt : Seg
      , msgRef : (Maybe Seg)
      , msgAttributee : (Maybe Attributee)
      }
  | MsgAbsent
      { msgPath : Path
      , msgTgt : Seg
      , msgAttributee : (Maybe Attributee)
      }

type ToRelayClientBundle = ToRelayClientBundle
    (List SubMsg) (List DataUpdateMsg) (List ContainerUpdateMsg)

-- Note: equivalent to haskell side `DefMessage TypeName`
type DefMsg
  = MsgDefine TypeName Definition
  | MsgUndefine TypeName

type TypeMsg = MsgAssignType Path TypeName Liberty

type FromRelayClientBundle = FromRelayClientBundle
    (List TypeName) (List Path) (List MsgError) (List DefMsg) (List TypeMsg)
    (List DataUpdateMsg) (List ContainerUpdateMsg)
