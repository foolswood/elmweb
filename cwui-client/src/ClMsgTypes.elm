module ClMsgTypes exposing (..)
import ClTypes exposing (Path, Attributee, Site, Time, Interpolation, ChildName, ClValue)

type SubMsg
  = MsgSub Path
  | MsgUnsub Path

type RequestBundle = RequestBundle (List SubMsg) (List DataUpdateMsg)

type DataUpdateMsg
  = MsgAdd
      { msgPath : Path
      , msgTime : Time
      , msgArgs : (List ClValue)
      , msgInterpolation : Interpolation
      , msgAttributee : (Maybe Attributee)
      , msgSite : (Maybe Site)
      }
  | MsgSet
      { msgPath : Path
      , msgTime : Time
      , msgArgs : List ClValue
      , msgInterpolation : Interpolation
      , msgAttributee : Maybe Attributee
      , msgSite : Maybe Site
      }
  | MsgRemove
      { msgPath : Path
      , msgTime : Time
      , msgAttributee : Maybe Attributee
      , msgSite : Maybe Site
      }
  | MsgClear
      { msgPath : Path
      , msgTime : Time
      , msgAttributee : Maybe Attributee
      , msgSite : Maybe Site
      }
  | MsgSetChildren
      { msgPath : Path
      , msgChildren : List ChildName
      , msgAttributee : Maybe Attributee
      }

type ErrorMsg = ErrorMsg Path String

type TreeUpdateMsg
  = MsgAssignType Path Path
  | MsgDelete Path

type UpdateMsg = TreeUpdateMsg TreeUpdateMsg | DataUpdateMsg DataUpdateMsg

type UpdateBundle = UpdateBundle (List ErrorMsg) (List UpdateMsg)
