module ClSpecParser exposing (parseBaseType)
import Dict exposing (..)

import ClTypes exposing (Path, Time, ClNode, ClType(..), ClValue(..), Liberty(..), Interpolation(..))
import Futility exposing (mapAllFaily)

zt : Time
zt = (0, 0)

clListToStrings : List ClValue -> Result String (List String)
clListToStrings =
  let
    rClString clv = case clv of
        (ClString s) -> Ok s
        _ -> Err "Value was not of string type"
  in
    mapAllFaily rClString

clListToInterps : List ClValue -> Result String (List Interpolation)
clListToInterps =
  let
    rInterp clv = case clv of
        (ClEnum e) -> case e of
            0 -> Ok IConstant
            1 -> Ok ILinear
            _ -> Err "Unrecognised interpolation enum value"
        _ -> Err "Interpolation ClValue not an enum"
  in
    mapAllFaily rInterp

clvToLiberty : ClValue -> Result String Liberty
clvToLiberty clv = case clv of
    (ClEnum e) -> case e of
        0 -> Ok Cannot
        1 -> Ok May
        2 -> Ok Must
        _ -> Err "Unrecognised value for liberty enum"
    _ -> Err "Liberty value not enum"

parseBaseType : Path -> ClNode -> Result String ClType
parseBaseType tp n = case tp of
    "/api/types/base/tuple" -> case Dict.get zt (.values n) of
        (Just [ClString doc, ClList vNames, ClList vAtomTypes, ClList vInterps]) ->
            Result.map3
                (\ns ats itps -> ClTuple {
                    doc = doc, names = ns, atomTypes = ats,
                    interpolations = itps})
                (clListToStrings vNames)
                (clListToStrings vAtomTypes)
                (clListToInterps vInterps)
        (Just _) -> Err "Invalid value types for type info"
        Nothing -> Err "No value at t=0 in type info"
    "/api/types/base/struct" -> case Dict.get zt (.values n) of
        (Just [ClString doc, ClList vChildNames, ClList vChildTypes, ClList vChildLibs]) ->
            Result.map3
                (\cn ct cl -> ClStruct {
                    doc = doc, childNames = cn, childTypes = ct,
                    childLiberties = cl})
                (clListToStrings vChildNames)
                (clListToStrings vChildTypes)
                (mapAllFaily clvToLiberty vChildLibs)
        (Just _) -> Err "Invalid value types for struct def"
        Nothing -> Err "No value at t=0 for struct def"
    "/api/types/base/array" -> case Dict.get zt (.values n) of
        (Just [ClString doc, ClString childType, ClEnum vLib]) -> Result.map
            (\l -> ClArray {doc = doc, childType = childType, childLiberty = l})
            (clvToLiberty (ClEnum vLib))
        (Just _) -> Err "Invalid value types for array def"
        Nothing -> Err "No value at t=0 for array def"
    _ -> Err "Not a base type path"
