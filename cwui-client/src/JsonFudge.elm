module JsonFudge exposing (serialiseBundle, parseBundle)
import Json.Encode as JE
import Json.Decode as JD

import ClTypes exposing (Path, Time, Interpolation(..), ClValue(..))
import ClMsgTypes exposing (..)

-- Json serialisation fudging

subMsgToJsonValue : SubMsg -> JE.Value
subMsgToJsonValue sm = JE.list (List.map JE.string (case sm of
    MsgSub p -> ["S", p]
    MsgUnsub p -> ["U", p]))

timeJson : Time -> JE.Value
timeJson (s, f) = JE.list [JE.int s, JE.int f]

tagged : Char -> JE.Value -> JE.Value
tagged t v = JE.list [JE.string (String.fromChar t), v]

encodeClValue : ClValue -> JE.Value
encodeClValue v = case v of
    (ClTime t) -> tagged 't' (timeJson t)
    (ClEnum e) -> tagged 'e' (JE.int e)
    (ClWord32 i) -> tagged 'u' (JE.int i)
    (ClWord64 i) -> tagged 'U' (JE.int i)
    (ClInt32 i) -> tagged 'i' (JE.int i)
    (ClInt64 i) -> tagged 'I' (JE.int i)
    (ClFloat f) -> tagged 'd' (JE.float f)
    (ClDouble f) -> tagged 'D' (JE.float f)
    (ClString s) -> tagged 's' (JE.string s)
    (ClList l) -> tagged 'l' (JE.list (List.map encodeClValue l))

dumToJsonValue : DataUpdateMsg -> JE.Value
dumToJsonValue dum =
  let
    encodeInterpolation i = case i of
        IConstant -> tagged 'C' (JE.list [])
        ILinear -> tagged 'L' (JE.list [])
    -- FIXME: no site/attributee
    tpiJson {msgPath, msgTime, msgArgs, msgInterpolation} = JE.object [
        ("path", JE.string msgPath)
      , ("time", timeJson msgTime)
      , ("args", JE.list (List.map encodeClValue msgArgs))
      , ("interpolation", encodeInterpolation msgInterpolation)]
    ptasJson {msgPath, msgTime, msgAttributee, msgSite} = JE.object [
        ("path", JE.string msgPath)
      , ("time", timeJson msgTime)]
  in
    case dum of
        MsgAdd tpi -> tagged 'a' (tpiJson tpi)
        MsgSet tpi -> tagged 's' (tpiJson tpi)
        MsgRemove ptas -> tagged 'r' (ptasJson ptas)
        MsgClear ptas -> tagged 'c' (ptasJson ptas)
        MsgSetChildren {msgPath, msgChildren} -> tagged 'S' (JE.object [
            ("path", JE.string msgPath), ("names", JE.list (List.map JE.string msgChildren))])

serialiseBundle : RequestBundle -> String
serialiseBundle (RequestBundle subs dums) = JE.encode 2 (JE.object [
    ("subs", JE.list (List.map subMsgToJsonValue subs)),
    ("dums", JE.list (List.map dumToJsonValue dums))])

decodePath : JD.Decoder String
decodePath = JD.string

decodeErrMsg : JD.Decoder ErrorMsg
decodeErrMsg = JD.map2 ErrorMsg (JD.field "path" decodePath) (JD.field "msg" JD.string)

decodeTime : JD.Decoder Time
decodeTime = JD.map2 (\a b -> (a, b)) (JD.index 0 JD.int) (JD.index 1 JD.int)

decodeClValue : JD.Decoder ClValue
decodeClValue =
  let
    decoderForClValueTag t = case t of
        "t" -> JD.map ClTime decodeTime
        "e" -> JD.map ClEnum JD.int
        "u" -> JD.map ClWord32 JD.int
        "U" -> JD.map ClWord64 JD.int
        "i" -> JD.map ClInt32 JD.int
        "I" -> JD.map ClInt64 JD.int
        "d" -> JD.map ClFloat JD.float
        "D" -> JD.map ClDouble JD.float
        "s" -> JD.map ClString JD.string
        "l" -> JD.map ClList (JD.list decodeClValue)
        _ -> JD.fail "unrecognised tag"
  in
    JD.andThen (JD.index 1 << decoderForClValueTag) (JD.index 0 JD.string)

decodeInterpolation : JD.Decoder Interpolation
decodeInterpolation =
  let
    decoderForTag t = case t of
        "C" -> JD.succeed IConstant
        "L" -> JD.succeed ILinear
        _ -> JD.fail "unrecognised interpolation tag"
  in
    JD.andThen decoderForTag (JD.index 0 JD.string)

decodeMsg : JD.Decoder UpdateMsg
decodeMsg =
  let
    decodePathField = JD.field "path" decodePath
    decodeTimeField = JD.field "time" decodeTime
    decodeArgsField = JD.field "args" (JD.list decodeClValue)
    decodeInterpolationField = JD.field "interpolation" decodeInterpolation
    decoderForTag t = case t of
        "A" -> JD.map2
            (\p tp -> TreeUpdateMsg (MsgAssignType p tp))
            decodePathField (JD.field "type" decodePath)
        "C" -> JD.map2
            (\p ns -> DataUpdateMsg (MsgSetChildren {
                msgPath=p, msgChildren=ns, msgAttributee=Nothing}))
            decodePathField (JD.field "names" (JD.list JD.string))
        "a" -> JD.map4
            (\p t a i -> DataUpdateMsg (MsgAdd {
                msgPath=p, msgTime=t, msgArgs=a, msgInterpolation=i,
                msgAttributee=Nothing, msgSite=Nothing}))
            decodePathField decodeTimeField decodeArgsField decodeInterpolationField
        "s" -> JD.map4
            (\p t a i -> DataUpdateMsg (MsgSet {
                msgPath=p, msgTime=t, msgArgs=a, msgInterpolation=i,
                msgAttributee=Nothing, msgSite=Nothing}))
            decodePathField decodeTimeField decodeArgsField decodeInterpolationField
        _ -> JD.fail "unrecognised msg tag"
  in
    JD.andThen (JD.index 1 << decoderForTag) (JD.index 0 JD.string)

parseBundle : String -> Result String UpdateBundle
parseBundle = JD.decodeString (JD.map2 UpdateBundle (JD.field "errs" (JD.list decodeErrMsg)) (JD.field "ups" (JD.list decodeMsg)))
