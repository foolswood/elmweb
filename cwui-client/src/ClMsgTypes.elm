module ClMsgTypes exposing (..)

import Futility exposing (Either)
import Tagged.Tagged exposing (Tagged)
import ClTypes exposing
  ( Path, Seg, Placeholder, Namespace, TypeName, SubPath, Attributee, TpId
  , Time, Interpolation, Definition, PostDefinition, Editable, WireValue
  , WireType)

type SubErrorIndex
  = SPathError SubPath
  | STypeError (Tagged Definition TypeName)
  | SPostTypeError (Tagged PostDefinition TypeName)

type DataErrorIndex
  = DGlobalError
  | DPathError Path
  | DTimePointError Path TpId
