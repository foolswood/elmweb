module PathManipulation exposing
  ( splitBasename, appendSeg, canonicalise, asPath, appendSegSp)

import Regex exposing (Regex)

import Tagged.Tagged exposing (Tagged(..))
import ClTypes exposing (Path, Seg, SubPath)

s : String
s = "/"

basenameRe : Regex
basenameRe = Regex.regex "^(/.+)/(\\w+)$"

splitBasename : Path -> Maybe (Path, Seg)
splitBasename p = case Regex.find Regex.All basenameRe p of
    [{submatches}] -> case submatches of
        [Just p, Just s] -> Just (p, s)
        _ -> Nothing
    _ -> Nothing

appendSeg : Path -> Seg -> Path
appendSeg p s = p ++ "/" ++ s

canonicalise : Path -> Path
canonicalise p = if String.endsWith "/" p
  then String.dropRight 1 p
  else p

asPath : List Seg -> Path
asPath segs = "/" ++ String.join "/" segs

appendSegSp : SubPath -> Seg -> SubPath
appendSegSp (Tagged (ns, p)) s = Tagged (ns, appendSeg p s)
