module ClTypes exposing (..)
import Dict exposing (..)

type alias Path = String
type alias ChildName = String
type alias Time = (Int, Int)
type alias Attributee = String
type alias Site = String

type Liberty
  = Cannot
  | May
  | Must

type Interpolation
  = IConstant
  | ILinear

type alias ClAtomType = String

type ClType
  = ClTuple {
        doc : String,
        names : List String,
        atomTypes : List ClAtomType,
        interpolations : List Interpolation} -- Can't define own types that go in a set (because comparable)
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

-- FIXME: nowhere to put tree structure!
type alias ClNode =
  { errors : List String
  , values : ClSeries
  , pending : ClSeries
  }

emptyNode : ClNode
emptyNode = ClNode [] Dict.empty Dict.empty

zt : Time
zt = (0, 0)
