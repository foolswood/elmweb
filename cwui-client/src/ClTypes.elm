module ClTypes exposing (..)
import Dict exposing (..)
import Regex exposing (Regex)

type alias Seg = String
type alias Path = String
type alias TypeName = (Seg, Seg)

type alias Attributee = String

type alias TpId = Int
type alias Time = (Int, Int)

fromFloat : Float -> Time
fromFloat ft = let s = floor ft in (s, round ((ft - toFloat s) * 2 ^ 32))

fromTime : Time -> Float
fromTime (s, f) = toFloat s + (toFloat f / 2.0 ^ 32)

type Liberty
  = Cannot
  | May
  | Must

type InterpolationLimit
  = ILUninterpolated
  | ILConstant
  | ILLinear

type Interpolation
  = IConstant
  | ILinear

type alias Bounds a = {minBound : Maybe a, maxBound : Maybe a}

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
  | ADRef TypeName
  | ADList AtomDef
  | ADSet AtomDef

type alias TupleDefinition =
  { doc : String
  , types : List (Seg, AtomDef)
  , interpLim : InterpolationLimit}

type alias ChildDescription = {name : Seg, typeRef : TypeName, lib : Liberty}
type alias StructDefinition = {doc : String, childDescs : List ChildDescription}
type alias ArrayDefinition = {doc : String, childType : TypeName, childLiberty : Liberty}

type Definition
  = TupleDef TupleDefinition
  | StructDef StructDefinition
  | ArrayDef ArrayDefinition

type WireValue
  = WvTime Time
  | WvWord8 Int
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

asWord8 : WireValue -> Result String Int
asWord8 wv = case wv of
    WvWord8 i -> Ok i
    _ -> Err "Not a Word8"

asFloat : WireValue -> Result String Float
asFloat wv = case wv of
    WvFloat f -> Ok f
    _ -> Err "Not a Float"

asString : WireValue -> Result String String
asString wv = case wv of
    WvString s -> Ok s
    _ -> Err "Not a string"

type WireType
  = WtTime
  | WtWord8
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
    ADEnum _ -> WtWord8
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
