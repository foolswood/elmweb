module ClSpecParser exposing (parseAtomDef)

import Tagged.Tagged exposing (Tagged(..))
import ClTypes exposing (AtomDef(..))
import Regex exposing (regex)

type alias ConstraintParser = (String, (String -> Result String AtomDef))

constraintParsers : List (ConstraintParser)
constraintParsers =
  let
    parseBounds fromStr s = if String.isEmpty s
        then Ok {minBound = Nothing, maxBound = Nothing}
        else case String.split ":" s of
            [minStr, maxStr] ->
              let
                fromMStr ms = case ms of
                    "" -> Ok Nothing
                    js -> Result.map Just <| fromStr js
                asBounds minB maxB = {minBound = minB, maxBound = maxB}
              in Result.map2 asBounds (fromMStr minStr) (fromMStr maxStr)
            _ -> Err <| "Invalid bounds: " ++ s
    timeBounds = parseBounds (always <| Err "Not implemented")
    intBounds = parseBounds String.toInt
    floatBounds = parseBounds String.toFloat
    cRegex s = Ok (s, regex s)
    enumOpts = Ok << String.split ","
    parsePath s = Ok s
    parseTypeName s = case String.split ":" s of
        [ns, seg] -> Ok (ns, seg)
        _ -> Err "Bad TypeName"
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
    ("ref", Ok << ADRef << Tagged)]

parseAtomDefWith : String -> List ConstraintParser -> Result String AtomDef
parseAtomDefWith s cps = case cps of
    [] -> Err (String.append "Unrecognised atom type: " s)
    (prefix, cp)::rcps -> if String.startsWith prefix s
        then cp (String.slice (String.length prefix + 1) (String.length s - 1) s)
        else parseAtomDefWith s rcps

parseAtomDef : String -> Result String AtomDef
parseAtomDef s = parseAtomDefWith s constraintParsers
