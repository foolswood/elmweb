module PathManipulation exposing (splitBasename)
import ClTypes exposing (Path, ChildName)

s : String
s = "/"

pathToSegs : Path -> List ChildName
pathToSegs = List.drop 1 << String.split "/"

segsToPath : List ChildName -> Path
segsToPath = String.append "/" << String.join "/"

splitBasename : Path -> Maybe (Path, ChildName)
splitBasename p = case List.reverse (pathToSegs p) of
    cn :: rSegs -> Just (segsToPath (List.reverse rSegs), cn)
    [] -> Nothing
