module PathManipulation exposing (splitBasename, appendSeg, canonicalise)
import ClTypes exposing (Path, Seg)

s : String
s = "/"

pathToSegs : Path -> List Seg
pathToSegs = List.drop 1 << String.split "/"

segsToPath : List Seg -> Path
segsToPath = String.append "/" << String.join "/"

splitBasename : Path -> Maybe (Path, Seg)
splitBasename p = case List.reverse (pathToSegs p) of
    cn :: rSegs -> Just (segsToPath (List.reverse rSegs), cn)
    [] -> Nothing

appendSeg : Path -> Seg -> Path
appendSeg p s = p ++ "/" ++ s

canonicalise : Path -> Path
canonicalise p = if String.endsWith "/" p
  then String.dropRight 1 p
  else p
