module ClSpecParser exposing (parseAtomDef)

import ClTypes exposing (AtomDef(..))

type alias ConstraintParser = (String, (String -> Result String AtomDef))

constraintParsers : List (ConstraintParser)
constraintParsers =
  let
    timeBounds s = if String.isEmpty s
        then Ok {minBound = Nothing, maxBound = Nothing}
        else Err "Bounds not implemented"
    enumOpts = Ok << String.split ","
    intBounds = timeBounds
    floatBounds = timeBounds
    cRegex s = Ok s
    parsePath s = Ok s
  in [
    ("time", Result.map ADTime << timeBounds),
    ("enum", Result.map ADEnum << enumOpts),
    ("word32", Result.map ADWord32 << intBounds),
    ("word64", Result.map ADWord64 << intBounds),
    ("int32", Result.map ADInt32 << intBounds),
    ("int64", Result.map ADInt64 << intBounds),
    ("float", Result.map ADFloat << floatBounds),
    ("double", Result.map ADDouble << floatBounds),
    ("string", Result.map ADString << cRegex),
    ("list", Result.map ADList << parseAtomDef),
    ("set", Result.map ADSet << parseAtomDef),
    ("ref", Result.map ADRef << parsePath)]

parseAtomDefWith : String -> List ConstraintParser -> Result String AtomDef
parseAtomDefWith s cps = case cps of
    [] -> Err (String.append "Unrecognised atom type: " s)
    (prefix, cp)::rcps -> if String.startsWith prefix s
        then cp (String.slice (String.length prefix + 1) (String.length s - 1) s)
        else parseAtomDefWith s rcps

parseAtomDef : String -> Result String AtomDef
parseAtomDef s = parseAtomDefWith s constraintParsers
