module ClMsgTypes exposing (..)
import ClTypes exposing (Path, Seg, Namespace, TypeName, Attributee, TpId, Time, Interpolation, Definition, PostDefinition, Liberty, WireValue, WireType)

type SubMsg
  = MsgSub Path
  | MsgTypeSub TypeName
  | MsgPostTypeSub TypeName
  | MsgUnsub Path
  | MsgTypeUnsub TypeName
  | MsgPostTypeUnsub TypeName

type SubErrorIndex
  = SPathError Path
  | STypeError TypeName
  | SPostTypeError TypeName

type DataErrorIndex
  = DGlobalError
  | DPathError Path
  | DTimePointError Path TpId

type MsgError a = MsgError a String

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

type ToClientContainerUpdateMsg
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

type alias PostArgs = List (WireType, WireValue)

type ToProviderContainerUpdateMsg
  = MsgCreateAfter
      { msgPath : Path
      , msgPostArgs : PostArgs
      , msgTgt : Seg
      , msgRef : (Maybe Seg)
      , msgAttributee : (Maybe Attributee)
      }
  | MsgMoveAfter
      { msgPath : Path
      , msgTgt : Seg
      , msgRef : (Maybe Seg)
      , msgAttributee : (Maybe Attributee)
      }
  | MsgDelete
      { msgPath : Path
      , msgTgt : Seg
      , msgAttributee : (Maybe Attributee)
      }

type ToRelaySubBundle = ToRelaySubBundle (List SubMsg)
type ToRelayUpdateBundle = ToRelayUpdateBundle
    Namespace
    (List DataUpdateMsg)
    (List ToProviderContainerUpdateMsg)

type ToRelayClientBundle = Trcub ToRelayUpdateBundle | Trcsb ToRelaySubBundle

type DefMsg a
  = MsgDefine Seg a
  | MsgUndefine Seg

type TypeMsg = MsgAssignType Path TypeName Liberty

type FromRelayClientUpdateBundle = FromRelayClientUpdateBundle
    Namespace
    (List (MsgError DataErrorIndex))
    (List (DefMsg PostDefinition))
    (List (DefMsg Definition))
    (List TypeMsg)
    (List DataUpdateMsg)
    (List ToClientContainerUpdateMsg)

type FromRelaySubErrorBundle = FromRelaySubErrorBundle
    (List (MsgError SubErrorIndex))
    (List TypeName) -- post unsubs
    (List TypeName) -- def unsubs
    (List Path)     -- path unsubs

type FromRelayRootBundle = FromRelayRootBundle (List ToClientContainerUpdateMsg)

type FromRelayClientBundle
  = Frcub FromRelayClientUpdateBundle
  | Frseb FromRelaySubErrorBundle
  | Frrub FromRelayRootBundle
