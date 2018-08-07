module ClMsgTypes exposing (..)
import ClTypes exposing (Path, Seg, Namespace, TypeName, Attributee, TpId, Time, Interpolation, Definition, PostDefinition, Editable, WireValue, WireType)

type alias SubPath = (Namespace, Path)

type SubMsg
  = MsgSub SubPath
  | MsgTypeSub TypeName
  | MsgPostTypeSub TypeName
  | MsgUnsub SubPath
  | MsgTypeUnsub TypeName
  | MsgPostTypeUnsub TypeName

type SubErrorIndex
  = SPathError SubPath
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
      { msgTgt : Seg
      , msgRef : (Maybe Seg)
      , msgAttributee : (Maybe Attributee)
      }
  | MsgAbsent
      { msgTgt : Seg
      , msgAttributee : (Maybe Attributee)
      }

type alias PostArgs = List (WireType, WireValue)

type ToProviderContainerUpdateMsg
  = MsgCreateAfter
      { msgPostArgs : PostArgs
      , msgTgt : Seg
      , msgRef : (Maybe Seg)
      , msgAttributee : (Maybe Attributee)
      }
  | MsgMoveAfter
      { msgTgt : Seg
      , msgRef : (Maybe Seg)
      , msgAttributee : (Maybe Attributee)
      }
  | MsgDelete
      { msgTgt : Seg
      , msgAttributee : (Maybe Attributee)
      }

type ToRelaySubBundle = ToRelaySubBundle (List SubMsg)
type ToRelayUpdateBundle = ToRelayUpdateBundle
    Namespace
    (List DataUpdateMsg)
    (List (Path, ToProviderContainerUpdateMsg))

type ToRelayClientBundle = Trcub ToRelayUpdateBundle | Trcsb ToRelaySubBundle

type DefMsg a
  = MsgDefine Seg a
  | MsgUndefine Seg

type TypeMsg = MsgAssignType Path Seg Editable

type FromRelayClientUpdateBundle = FromRelayClientUpdateBundle
    Namespace
    (List (MsgError DataErrorIndex))
    (List (DefMsg PostDefinition))
    (List (DefMsg Definition))
    (List TypeMsg)
    (List DataUpdateMsg)
    (List (Path, ToClientContainerUpdateMsg))

type FromRelaySubErrorBundle = FromRelaySubErrorBundle
    (List (MsgError SubErrorIndex))
    (List TypeName) -- post unsubs
    (List TypeName) -- def unsubs
    (List SubPath)     -- path unsubs

type FromRelayRootBundle = FromRelayRootBundle (List ToClientContainerUpdateMsg)

type FromRelayClientBundle
  = Frcub FromRelayClientUpdateBundle
  | Frseb FromRelaySubErrorBundle
  | Frrub FromRelayRootBundle
