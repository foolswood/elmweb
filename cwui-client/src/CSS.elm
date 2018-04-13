module CSS exposing (emPx, Styles, KeyFrames, KeyFramesDict, KeyFramed, keyFramed, applyKeyFramed)

import Dict exposing (Dict)

import Native.CSS

emPx : () -> Float
emPx = Native.CSS.emPx

insertRule : String -> Int -> ()
insertRule rule idx = Native.CSS.insertRule {rule = rule, idx = idx}

deleteRule : Int -> ()
deleteRule = Native.CSS.deleteRule

-- FIXME: Not really MVars, more like a mutable dict.
swapKeyFrames : KeyFramesDict -> Maybe KeyFramesDict
swapKeyFrames kfd =
  let
    rv = Native.CSS.swapMvar {key = "keyFrames", value = kfd}
  in case .hasValue rv of
    True -> Just <| .value rv
    False -> Nothing

-- Actual elm:

type RuleOp
  = InsertRule Int String
  | DeleteRule Int

type alias Styles = Dict String String
type alias KeyFrames = Dict Int Styles
type alias KeyFramesDict = Dict String KeyFrames

stylesStr : Styles -> String
stylesStr =
  let
    styleStr k v acc = (k ++ ":" ++ v) :: acc
  in String.join ";" << Dict.foldl styleStr []

insertKeyFrame : Int -> String -> KeyFrames -> RuleOp
insertKeyFrame idx name kfs =
  let
    frameStr k s acc = (toString k ++ "% {" ++ stylesStr s ++ "}") :: acc
    framesStr = String.join " " <| Dict.foldr frameStr [] kfs
  in InsertRule idx <| "@keyframes " ++ name ++ " {" ++ framesStr ++ "}"

diffKeyFramesDict : KeyFramesDict -> KeyFramesDict -> (List RuleOp)
diffKeyFramesDict current new =
  let
    oldKeyFramesDict n _ (ops, idx) = (ops ++ [DeleteRule idx], idx)
    bothKeyFramesDict n fc fn (ops, idx) = if fc == fn
        then (ops, idx + 1)
        else (ops ++ [DeleteRule idx, insertKeyFrame idx n fn], idx + 1)
    newKeyFramesDict n f (ops, idx) = (ops ++ [insertKeyFrame idx n f], idx + 1)
    (ruleOps, _) = Dict.merge
        oldKeyFramesDict
        bothKeyFramesDict
        newKeyFramesDict
        current
        new
        ([], 0)
  in ruleOps

applyOps : List RuleOp -> ()
applyOps ops =
  let
    applyOp op _ = case op of
        InsertRule idx ruleStr -> insertRule ruleStr idx
        DeleteRule idx -> deleteRule idx
    applied = List.foldl applyOp () ops
  in ()

type KeyFramed a = KeyFramed KeyFramesDict a

keyFramed : KeyFramesDict -> a -> KeyFramed a
keyFramed = KeyFramed

applyKeyFramed : KeyFramed a -> a
applyKeyFramed (KeyFramed kfd a) =
  let
    ckfd = Maybe.withDefault Dict.empty <| swapKeyFrames kfd
    ops = diffKeyFramesDict ckfd kfd
    _ = applyOps ops
  in a
