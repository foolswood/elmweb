module ClTypes exposing (..)
import Dict exposing (..)

type alias Path = String
type alias ChildName = String
type alias Attributee = String
type alias Site = String

type alias Time = (Int, Int)

fromFloat : Float -> Time
fromFloat ft = let s = floor ft in (s, round ((ft - toFloat s) * 2 ^ 32))

type Liberty
  = Cannot
  | May
  | Must

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
  | ADString String
  | ADList AtomDef
  | ADSet AtomDef
  | ADRef String
  | ADValidator

type alias ClTupleType = {
    doc : String,
    names : List String,
    atomTypes : List AtomDef,
    interpolations : List Interpolation} -- Can't define own types that go in a set (because comparable)

type ClType
  = ClTuple ClTupleType
  | ClStruct {doc : String, childNames: List ChildName, childTypes : List Path, childLiberties : List Liberty}
  | ClArray {doc : String, childType : Path, childLiberty : Liberty}

type ClValue
  = ClTime Time
  | ClEnum Int
  | ClWord32 Int
  | ClWord64 Int
  | ClInt32 Int
  | ClInt64 Int
  | ClFloat Float
  | ClDouble Float
  | ClString String
  | ClList (List ClValue)

type alias ClSeries = Dict Time (List ClValue)

type alias ClTupleNode =
  { values : ClSeries
  , pending : ClSeries
  }

type alias ClContainerNode =
  { children : List ChildName
  }

type ClNodeT
  = TupleNode ClTupleNode
  | ContainerNode ClContainerNode
  | UnpopulatedNode

-- FIXME: nowhere to put tree structure!
type alias ClNode =
  { errors : List String
  , body : ClNodeT
  }

emptyNode : ClNode
emptyNode = ClNode [] UnpopulatedNode

emptyTupleNode : ClNodeT
emptyTupleNode = TupleNode {values = Dict.empty, pending = Dict.empty}

emptyContainerNode : ClNodeT
emptyContainerNode = ContainerNode {children = []}
