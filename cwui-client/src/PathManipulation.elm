module PathManipulation exposing (splitBasename)
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
