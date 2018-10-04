module ClTypes exposing (..)
import Dict exposing (Dict)
import Regex exposing (Regex)

import Tagged.Tagged exposing (Tagged(..))

type NsTag = NsTag
type PhTag = PhTag

type alias Seg = String
type alias Namespace = Tagged NsTag Seg
type alias Placeholder = Tagged PhTag Seg
type alias Path = String
type alias TypeName = (Seg, Seg)
type alias SubPath = Tagged NsTag (Seg, Path)

typeName : Namespace -> Tagged a Seg -> Tagged a TypeName
typeName (Tagged ns) (Tagged s) = Tagged (ns, s)

typeNameGetNs : Tagged a TypeName -> Namespace
typeNameGetNs (Tagged (ns, _)) = Tagged ns

typeNameGetSeg : Tagged a TypeName -> Tagged a Seg
typeNameGetSeg (Tagged (_, s)) = Tagged s

subPath : Namespace -> Path -> SubPath
subPath (Tagged ns) p = Tagged <| (ns, p)

unSubPath : SubPath -> (Namespace, Path)
unSubPath (Tagged (ns, p)) = (Tagged ns, p)

type alias Attributee = String

type alias TpId = Int
type alias Time = (Int, Int)

fromFloat : Float -> Time
fromFloat ft = let s = floor ft in (s, round ((ft - toFloat s) * 2 ^ 32))

fromTime : Time -> Float
fromTime (s, f) = toFloat s + (toFloat f / 2.0 ^ 32)

type Editable
  = ReadOnly
  | Editable

type InterpolationLimit
  = ILUninterpolated
  | ILConstant
  | ILLinear

type Interpolation
  = IConstant
  | ILinear

type alias Bounds a = {minBound : Maybe a, maxBound : Maybe a}

unbounded : Bounds a
unbounded = {minBound = Nothing, maxBound = Nothing}

type AtomDef
  = ADTime (Bounds Time)
  | ADEnum (List String)
  | ADWord32 (Bounds Int)
  | ADWord64 (Bounds Int)
  | ADInt32 (Bounds Int)
  | ADInt64 (Bounds Int)
  | ADFloat (Bounds Float)
  | ADDouble (Bounds Float)
  | ADString (String, Regex)
  | ADRef (Tagged Definition Seg)
  | ADList AtomDef
  | ADSet AtomDef

type alias TupleDefinition =
  { doc : String
  , types : List (Seg, AtomDef)
  , interpLim : InterpolationLimit}

type alias ChildDescription = {name : Seg, typeRef : Seg, ed : Editable}
type alias StructDefinition = {doc : String, childDescs : List ChildDescription}
type alias ArrayDefinition =
  { doc : String
  , postType : Maybe (Tagged PostDefinition Seg)
  , childType : Seg
  , childEditable : Editable}

type Definition
  = TupleDef TupleDefinition
  | StructDef StructDefinition
  | ArrayDef ArrayDefinition

type alias PostDefinition = {doc : String, fieldDescs : List (Seg, List AtomDef)}

type WireValue
  = WvTime Time
  | WvWord32 Int
  | WvWord64 Int
  | WvInt32 Int
  | WvInt64 Int
  | WvFloat Float
  | WvDouble Float
  | WvString String
  | WvList (List WireValue)

asTime : WireValue -> Result String Time
asTime wv = case wv of
    WvTime t -> Ok t
    _ -> Err "Not a Time"

asInt32 : WireValue -> Result String Int
asInt32 wv = case wv of
    WvInt32 i -> Ok i
    _ -> Err "Not an Int32"

asInt64 : WireValue -> Result String Int
asInt64 wv = case wv of
    WvInt64 i -> Ok i
    _ -> Err "Not an Int64"

asWord32 : WireValue -> Result String Int
asWord32 wv = case wv of
    WvWord32 w -> Ok w
    _ -> Err "Not a Word32"

asFloat : WireValue -> Result String Float
asFloat wv = case wv of
    WvFloat f -> Ok f
    _ -> Err "Not a Float"

asDouble : WireValue -> Result String Float
asDouble wv = case wv of
    WvDouble f -> Ok f
    _ -> Err "Not a double"

asString : WireValue -> Result String String
asString wv = case wv of
    WvString s -> Ok s
    _ -> Err "Not a string"

type WireType
  = WtTime
  | WtWord32
  | WtWord64
  | WtInt32
  | WtInt64
  | WtFloat
  | WtDouble
  | WtString
  | WtList WireType

defWireType : AtomDef -> WireType
defWireType def = case def of
    ADTime _ -> WtTime
    ADEnum _ -> WtWord32
    ADWord32 _ -> WtWord32
    ADWord64 _ -> WtWord64
    ADInt32 _ -> WtInt32
    ADInt64 _ -> WtInt64
    ADFloat _ -> WtFloat
    ADDouble _ -> WtDouble
    ADString _ -> WtString
    ADRef _ -> WtString
    ADList subDef -> WtList <| defWireType subDef
    ADSet subDef -> WtList <| defWireType subDef

type SubErrorIndex
  = SPathError SubPath
  | STypeError (Tagged Definition TypeName)
  | SPostTypeError (Tagged PostDefinition TypeName)

type DataErrorIndex
  = DNsError
  | DPathError Path
  | DTimePointError Path TpId
