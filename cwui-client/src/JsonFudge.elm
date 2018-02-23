module JsonFudge exposing (serialiseBundle, parseBundle)
import Json.Encode as JE
import Json.Decode as JD
import Dict

import ClTypes exposing
  ( Path, Time, Interpolation(..), TpId, Seg, Attributee
  , TypeName, Liberty(..), Definition(..), AtomDef, InterpolationLimit(..)
  , ChildDescription, WireValue(..), WireType(..))
import ClSpecParser exposing (parseAtomDef)
import ClMsgTypes exposing (..)

-- Json serialisation fudging

encodeNullable : (a -> JE.Value) -> Maybe a -> JE.Value
encodeNullable encA ma = case ma of
    Just a -> encA a
    Nothing -> JE.null

encodeSeg : Seg -> JE.Value
encodeSeg = JE.string

encodePath : Path -> JE.Value
encodePath = JE.string

encodeTypeName : TypeName -> JE.Value
encodeTypeName (ns, seg) = JE.object
  [ ("ns", encodeSeg ns)
  , ("seg", encodeSeg seg)]

encodeSubMsg : SubMsg -> JE.Value
encodeSubMsg sm = JE.list (case sm of
    MsgSub p -> [JE.string "s", encodePath p]
    MsgTypeSub tn -> [JE.string "S", encodeTypeName tn]
    MsgUnsub p -> [JE.string "u", encodePath p]
    MsgTypeUnsub tn -> [JE.string "U", encodeTypeName tn])

encodeTime : Time -> JE.Value
encodeTime (s, f) = JE.list [JE.int s, JE.int f]

tagged : Char -> JE.Value -> JE.Value
tagged t v = JE.list [JE.string (String.fromChar t), v]

encodeWv : WireType -> WireValue -> JE.Value
encodeWv wt wv =
  let
    encodeWv v = case v of
        (WvTime t) -> encodeTime t
        (WvWord8 i) -> JE.int i
        (WvWord32 i) -> JE.int i
        (WvWord64 i) -> JE.int i
        (WvInt32 i) -> JE.int i
        (WvInt64 i) -> JE.int i
        (WvFloat f) -> JE.float f
        (WvDouble f) -> JE.float f
        (WvString s) -> JE.string s
        (WvList l) -> JE.list (List.map encodeWv l)
    typeTag t = case t of
        WtTime -> "t"
        WtWord8 -> "b"
        WtWord32 -> "w"
        WtWord64 -> "W"
        WtInt32 -> "i"
        WtInt64 -> "I"
        WtFloat -> "f"
        WtDouble -> "F"
        WtString -> "s"
        WtList subT -> "l" ++ typeTag subT
  in JE.object
    [ ("type", JE.string (typeTag wt))
    , ("val", encodeWv wv)]

encodePathField : Path -> (String, JE.Value)
encodePathField p = ("path", JE.string p)

encodeAttributeeField : Maybe Attributee -> (String, JE.Value)
encodeAttributeeField ma = ("att", encodeNullable JE.string ma)

dumToJsonValue : DataUpdateMsg -> JE.Value
dumToJsonValue dum =
  let
    encodeInterpolation i = case i of
        IConstant -> tagged 'C' (JE.list [])
        ILinear -> tagged 'L' (JE.list [])
    encodeTpIdField tpid = ("tpid", JE.int tpid)
    encodeArgsField types args = ("args", JE.list (List.map2 encodeWv types args))
    encodeConstSet {msgPath, msgTypes, msgArgs, msgAttributee} = JE.object
        [ encodePathField msgPath
        , encodeArgsField msgTypes msgArgs
        , encodeAttributeeField msgAttributee
        ]
    encodeSet {msgPath, msgTpId, msgTime, msgTypes, msgArgs, msgInterpolation, msgAttributee} = JE.object
        [ encodePathField msgPath
        , encodeTpIdField msgTpId
        , ("time", encodeTime msgTime)
        , encodeArgsField msgTypes msgArgs
        , ("interpolation", encodeInterpolation msgInterpolation)
        , encodeAttributeeField msgAttributee
        ]
    encodeRemove {msgPath, msgTpId, msgAttributee} = JE.object
        [ encodePathField msgPath
        , encodeTpIdField msgTpId
        , encodeAttributeeField msgAttributee
        ]
  in
    case dum of
        MsgConstSet m -> tagged 'S' (encodeConstSet m)
        MsgSet m -> tagged 's' (encodeSet m)
        MsgRemove m -> tagged 'r' (encodeRemove m)

contToJsonValue : ContainerUpdateMsg -> JE.Value
contToJsonValue m =
  let
    encodeTgtField tgt = ("tgt", encodeSeg tgt)
  in case m of
    MsgPresentAfter {msgPath, msgTgt, msgRef, msgAttributee} -> JE.object
      [ encodePathField msgPath
      , encodeTgtField msgTgt
      , ("ref", encodeNullable encodeSeg msgRef)
      , encodeAttributeeField msgAttributee
      ]
    MsgAbsent {msgPath, msgTgt, msgAttributee} -> JE.object
      [ encodePathField msgPath
      , encodeTgtField msgTgt
      , encodeAttributeeField msgAttributee
      ]

serialiseBundle : ToRelayClientBundle -> Time -> String
serialiseBundle (ToRelayClientBundle subs dums conts) t = JE.encode 2 (JE.object
  [ ("subs", JE.list (List.map encodeSubMsg subs))
  , ("data", JE.list (List.map dumToJsonValue dums))
  , ("cont", JE.list (List.map contToJsonValue conts))
  , ("time", encodeTime t)])

decodePath : JD.Decoder String
decodePath = JD.string

decodeErrIdx : JD.Decoder ErrorIndex
decodeErrIdx = decodeTagged (Dict.fromList
    [ ("g", JD.succeed GlobalError)
    , ("p", JD.map PathError decodePath)
    , ("t", JD.map2 TimePointError decodePath decodeTpId)
    , ("n", JD.map TypeError decodeTypeName)
    ])

decodeErrMsg : JD.Decoder MsgError
decodeErrMsg = JD.map2 MsgError (JD.field "eIdx" decodeErrIdx) (JD.field "msg" JD.string)

decodeTime : JD.Decoder Time
decodeTime = JD.map2 (\a b -> (a, b)) (JD.index 0 JD.int) (JD.index 1 JD.int)

decodeTagged : Dict.Dict String (JD.Decoder a) -> JD.Decoder a
decodeTagged dm =
  let
    decoderForTag t = case Dict.get t dm of
        Just d -> d
        Nothing -> JD.fail ("Unrecognised tag: " ++ t)
  in JD.andThen (JD.index 1 << decoderForTag) (JD.index 0 JD.string)

decodeDocField : JD.Decoder String
decodeDocField = JD.field "doc" JD.string

decodeAtomDef : JD.Decoder AtomDef
decodeAtomDef =
  let
    pd s = case parseAtomDef s of
        Ok ad -> JD.succeed ad
        Err s -> JD.fail s
  in JD.andThen pd JD.string

decodeInterpolationLimit : JD.Decoder InterpolationLimit
decodeInterpolationLimit =
  let
    iltd = Dict.fromList
      [ ("U", JD.succeed ILUninterpolated)
      , ("C", JD.succeed ILConstant)
      , ("L", JD.succeed ILLinear)
      ]
  in decodeTagged iltd

decodeChildDesc : JD.Decoder ChildDescription
decodeChildDesc = JD.map3 (\n t l -> {name = n, typeRef = t, lib = l})
    (JD.field "seg" decodeSeg)
    (JD.field "tn" decodeTypeName)
    (JD.field "lib" decodeLiberty)

defTagDecoders : Dict.Dict String (JD.Decoder Definition)
defTagDecoders = Dict.fromList
  [ ("T", JD.map3
      (\d ts il -> TupleDef {doc = d, types = ts, interpLim = il})
      decodeDocField
      (JD.field "types" (JD.list (JD.map2 (,) (JD.field "seg" decodeSeg) (JD.field "ty" decodeAtomDef))))
      (JD.field "il" decodeInterpolationLimit)
    )
  , ("S", JD.map2
      (\d cds -> StructDef {doc = d, childDescs = cds})
      decodeDocField
      (JD.field "stls" (JD.list decodeChildDesc)))
  , ("A", JD.map3
      (\d ctn ctl -> ArrayDef {doc = d, childType = ctn, childLiberty = ctl})
      decodeDocField
      (JD.field "ctn" decodeTypeName)
      (JD.field "clib" decodeLiberty))
  ]

decodeDef : JD.Decoder Definition
decodeDef = decodeTagged defTagDecoders

decodeDefMsg : JD.Decoder DefMsg
decodeDefMsg = decodeTagged (Dict.fromList
  [ ("d", JD.map2 MsgDefine (JD.field "id" decodeTypeName) (JD.field "def" decodeDef))
  , ("u", JD.map MsgUndefine decodeTypeName)
  ])

decodeLiberty : JD.Decoder Liberty
decodeLiberty =
  let
    libFromString s = case s of
        "cannot" -> JD.succeed Cannot
        "may" -> JD.succeed May
        "must" -> JD.succeed Must
        _ -> JD.fail ("Unrecognised liberty string: " ++ s)
  in JD.andThen libFromString JD.string

decodeWv : JD.Decoder (WireType, WireValue)
decodeWv =
  let
    unpackWt wts = case String.uncons wts of
        Just (awt, rem) -> case awt of
            't' -> Ok WtTime
            'b' -> Ok WtWord8
            'w' -> Ok WtWord32
            'W' -> Ok WtWord64
            'i' -> Ok WtInt32
            'I' -> Ok WtInt64
            'f' -> Ok WtFloat
            'F' -> Ok WtDouble
            's' -> Ok WtString
            'l' -> Result.map WtList (unpackWt rem)
            _ -> Err "Unrecognised WireType tag"
        Nothing -> Err "Abruptly terminated WireType tags string"
    decodeWt wts = case unpackWt wts of
        Ok wt -> JD.succeed wt
        Err msg -> JD.fail msg
    wvd wt = case wt of
        WtTime -> JD.map WvTime decodeTime
        WtWord8 -> JD.map WvWord8 JD.int
        WtWord32 -> JD.map WvWord32 JD.int
        WtWord64 -> JD.map WvWord64 JD.int
        WtInt32 -> JD.map WvInt32 JD.int
        WtInt64 -> JD.map WvInt64 JD.int
        WtFloat -> JD.map WvFloat JD.float
        WtDouble -> JD.map WvFloat JD.float
        WtString -> JD.map WvString JD.string
        WtList swt -> JD.map WvList (JD.list (wvd swt))
    dwv wt = JD.map (\wv -> (wt, wv)) (wvd wt)
  in JD.andThen (JD.field "val" << dwv) (JD.andThen decodeWt (JD.field "type" JD.string))

decodeInterpolation : JD.Decoder Interpolation
decodeInterpolation = decodeTagged <| Dict.fromList
  [ ("C", JD.succeed IConstant)
  , ("L", JD.succeed ILinear)
  ]

decodeTpId : JD.Decoder TpId
decodeTpId = JD.int

decodeAttributee : JD.Decoder Attributee
decodeAttributee = JD.string

decodePathField : JD.Decoder Path
decodePathField = JD.field "path" decodePath

decodeAttributeeField : JD.Decoder (Maybe Attributee)
decodeAttributeeField = JD.field "att" (JD.nullable decodeAttributee)

decodeSeg : JD.Decoder Seg
decodeSeg = JD.string

decodeTypeName : JD.Decoder TypeName
decodeTypeName = JD.map2 (,)
    (JD.field "ns" decodeSeg) (JD.field "seg" decodeSeg)

decodeTypeMsg : JD.Decoder TypeMsg
decodeTypeMsg = JD.map3 MsgAssignType
    decodePathField
    (JD.field "typeName" decodeTypeName)
    (JD.field "lib" decodeLiberty)

decodeDum : JD.Decoder DataUpdateMsg
decodeDum =
  let
    decodeArgsField = JD.field "args" (JD.map List.unzip (JD.list decodeWv))
    decodeTpIdField = JD.field "tpid" decodeTpId
    decodeInterpolationField = JD.field "interpolation" decodeInterpolation
  in decodeTagged <| Dict.fromList
    [ ("S", JD.map3
        (\p (ts, vs) att -> MsgConstSet
          { msgPath = p
          , msgTypes = ts
          , msgArgs = vs
          , msgAttributee = att})
        decodePathField
        decodeArgsField
        decodeAttributeeField)
    , ("s", JD.map6
        (\p tpid t (ts, vs) i a -> MsgSet
          { msgPath=p, msgTpId=tpid, msgTime=t, msgTypes=ts
          , msgArgs=vs, msgInterpolation=i, msgAttributee=a})
        decodePathField
        decodeTpIdField
        (JD.field "time" decodeTime)
        decodeArgsField
        decodeInterpolationField
        decodeAttributeeField)
    , ("r", JD.map3
        (\p tpid att -> MsgRemove
          {msgPath = p, msgTpId = tpid, msgAttributee = att})
        decodePathField
        decodeTpIdField
        decodeAttributeeField)
    ]

decodeCm : JD.Decoder ContainerUpdateMsg
decodeCm =
  let
    decodeTgtField = JD.field "tgt" decodeSeg
  in decodeTagged <| Dict.fromList
    [ (">", JD.map4
          (\p t r a -> MsgPresentAfter
            {msgPath = p, msgTgt = t, msgRef = r, msgAttributee = a})
          decodePathField
          decodeTgtField
          (JD.field "ref" (JD.nullable decodeSeg))
          decodeAttributeeField)
    , ("-", JD.map3
          (\p t a -> MsgAbsent
            {msgPath = p, msgTgt = t, msgAttributee = a})
          decodePathField
          decodeTgtField
          decodeAttributeeField)
    ]

parseBundle : String -> Result String FromRelayClientBundle
parseBundle = JD.decodeString (JD.map7 FromRelayClientBundle
    (JD.field "tu" (JD.list decodeTypeName))
    (JD.field "du" (JD.list decodePath))
    (JD.field "errs" (JD.list decodeErrMsg))
    (JD.field "defs" (JD.list decodeDefMsg))
    (JD.field "tas" (JD.list decodeTypeMsg))
    (JD.field "dd" (JD.list decodeDum))
    (JD.field "co" (JD.list decodeCm)))
