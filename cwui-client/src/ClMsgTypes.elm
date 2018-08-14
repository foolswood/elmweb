module ClMsgTypes exposing (..)

import Tagged.Tagged exposing (Tagged)
import ClTypes exposing (Path, Seg, Namespace, TypeName, SubPath, Attributee, TpId, Time, Interpolation, Definition, PostDefinition, Editable, WireValue, WireType)

type SubMsg
  = MsgSub SubPath
  | MsgTypeSub (Tagged Definition TypeName)
  | MsgPostTypeSub (Tagged PostDefinition TypeName)
  | MsgUnsub SubPath
  | MsgTypeUnsub (Tagged Definition TypeName)
  | MsgPostTypeUnsub (Tagged PostDefinition TypeName)

type SubErrorIndex
  = SPathError SubPath
  | STypeError (Tagged Definition TypeName)
  | SPostTypeError (Tagged PostDefinition TypeName)

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
  = MsgDefine (Tagged a Seg) a
  | MsgUndefine (Tagged a Seg)

type TypeMsg = MsgAssignType Path (Tagged Definition Seg) Editable

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
    (List (Tagged PostDefinition TypeName)) -- post unsubs
    (List (Tagged Definition TypeName)) -- def unsubs
    (List SubPath)     -- path unsubs

type FromRelayRootBundle = FromRelayRootBundle (List ToClientContainerUpdateMsg)

type FromRelayClientBundle
  = Frcub FromRelayClientUpdateBundle
  | Frseb FromRelaySubErrorBundle
  | Frrub FromRelayRootBundle
