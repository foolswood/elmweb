module JsonFudge exposing (serialiseBundle, parseBundle)
import Json.Encode as JE
import Json.Decode as JD
import Dict

import ClTypes exposing
  ( Path, Time, Interpolation(..), TpId, Seg, Attributee
  , TypeName, Liberty(..), Definition(..), PostDefinition, AtomDef
  , InterpolationLimit(..) , ChildDescription, WireValue(..), WireType(..))
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
encodeSubMsg sm = JE.list <| case sm of
    MsgSub p -> [JE.string "s", encodePath p]
    MsgTypeSub tn -> [JE.string "S", encodeTypeName tn]
    MsgPostTypeSub tn -> [JE.string "pS", encodeTypeName tn]
    MsgUnsub p -> [JE.string "u", encodePath p]
    MsgTypeUnsub tn -> [JE.string "U", encodeTypeName tn]
    MsgPostTypeUnsub tn -> [JE.string "pU", encodeTypeName tn]

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

encodeTgtField : Seg -> (String, JE.Value)
encodeTgtField tgt = ("tgt", encodeSeg tgt)

encodeRefField : Maybe Seg -> (String, JE.Value)
encodeRefField ref = ("ref", encodeNullable encodeSeg ref)

clientContToJsonValue : ToClientContainerUpdateMsg -> JE.Value
clientContToJsonValue m = case m of
    MsgPresentAfter {msgPath, msgTgt, msgRef, msgAttributee} -> tagged '>' <| JE.object
      [ encodePathField msgPath
      , encodeTgtField msgTgt
      , encodeRefField msgRef
      , encodeAttributeeField msgAttributee
      ]
    MsgAbsent {msgPath, msgTgt, msgAttributee} -> tagged '-' <| JE.object
      [ encodePathField msgPath
      , encodeTgtField msgTgt
      , encodeAttributeeField msgAttributee
      ]

providerContToJsonValue : ToProviderContainerUpdateMsg -> JE.Value
providerContToJsonValue m = case m of
    MsgCreateAfter {msgPath, msgPostArgs, msgTgt, msgRef, msgAttributee} -> tagged '+' <| JE.object
      [ encodePathField msgPath
      , ("args", JE.list <| List.map (uncurry encodeWv) msgPostArgs)
      , encodeTgtField msgTgt
      , encodeRefField msgRef
      , encodeAttributeeField msgAttributee
      ]
    MsgMoveAfter {msgPath, msgTgt, msgRef, msgAttributee} -> tagged '>' <| JE.object
      [ encodePathField msgPath
      , encodeTgtField msgTgt
      , encodeRefField msgRef
      , encodeAttributeeField msgAttributee
      ]
    MsgDelete {msgPath, msgTgt, msgAttributee} -> tagged '-' <| JE.object
      [ encodePathField msgPath
      , encodeTgtField msgTgt
      , encodeAttributeeField msgAttributee
      ]

serialiseUpdateBundle : ToRelayUpdateBundle -> Time -> String
serialiseUpdateBundle (ToRelayUpdateBundle ns dums conts) t = JE.encode 2 (JE.object
  [ ("ns", JE.string ns)
  , ("data", JE.list (List.map dumToJsonValue dums))
  , ("cont", JE.list (List.map providerContToJsonValue conts))
  , ("time", encodeTime t)])

serialiseSubBundle : ToRelaySubBundle -> Time -> String
serialiseSubBundle (ToRelaySubBundle subs) t = JE.encode 2 (JE.object
  [ ("subMsgs", JE.list (List.map encodeSubMsg subs))
  , ("time", encodeTime t)])

serialiseBundle : ToRelayClientBundle -> Time -> String
serialiseBundle b = case b of
    Trcub ub -> serialiseUpdateBundle ub
    Trcsb sb -> serialiseSubBundle sb

decodePath : JD.Decoder String
decodePath = JD.string

decodeDataErrIdx : JD.Decoder DataErrorIndex
decodeDataErrIdx = decodeTagged (Dict.fromList
    [ ("g", JD.succeed DGlobalError)
    , ("p", JD.map DPathError decodePath)
    , ("t", JD.map2 DTimePointError decodePath decodeTpId)
    ])

decodeSubErrIdx : JD.Decoder SubErrorIndex
decodeSubErrIdx = decodeTagged (Dict.fromList
    [ ("p", JD.map SPathError decodePath)
    , ("t", JD.map STypeError decodeTypeName)
    , ("c", JD.map SPostTypeError decodeTypeName)
    ])

decodeErrMsg : JD.Decoder idx -> JD.Decoder (MsgError idx)
decodeErrMsg idxDec = JD.map2 MsgError (JD.field "eIdx" idxDec) (JD.field "msg" JD.string)

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

decodePostDef : JD.Decoder PostDefinition
decodePostDef = JD.map2 PostDefinition
    (JD.field "doc" JD.string)
    (JD.field "fields" <| JD.list <| JD.map2 (,)
        (JD.field "name" JD.string)
        (JD.field "type" decodeTypeName))

decodeDefMsg : JD.Decoder a -> JD.Decoder (DefMsg a)
decodeDefMsg defDec = decodeTagged (Dict.fromList
  [ ("d", JD.map2 MsgDefine (JD.field "id" decodeSeg) (JD.field "def" defDec))
  , ("u", JD.map MsgUndefine decodeSeg)
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

decodeCCm : JD.Decoder ToClientContainerUpdateMsg
decodeCCm =
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

parseRootBundle : JD.Decoder FromRelayRootBundle
parseRootBundle = JD.map FromRelayRootBundle <| JD.list decodeCCm

parseSubBundle : JD.Decoder FromRelaySubErrorBundle
parseSubBundle = JD.map4 FromRelaySubErrorBundle
    (JD.field "errs" (JD.list <| decodeErrMsg decodeSubErrIdx))
    (JD.field "ptuns" (JD.list decodeTypeName))
    (JD.field "tuns" (JD.list decodeTypeName))
    (JD.field "duns" (JD.list decodePath))

parseUpdateBundle : JD.Decoder FromRelayClientUpdateBundle
parseUpdateBundle = JD.map7 FromRelayClientUpdateBundle
    (JD.field "ns" JD.string)
    (JD.field "errs" (JD.list <| decodeErrMsg decodeDataErrIdx))
    (JD.field "pdefs" (JD.list <| decodeDefMsg decodePostDef))
    (JD.field "defs" (JD.list <| decodeDefMsg decodeDef))
    (JD.field "tas" (JD.list decodeTypeMsg))
    (JD.field "dd" (JD.list decodeDum))
    (JD.field "co" (JD.list decodeCCm))

parseBundle : String -> Result String FromRelayClientBundle
parseBundle = JD.decodeString <| decodeTagged <| Dict.fromList
  [ ("R", JD.map Frrub parseRootBundle)
  , ("S", JD.map Frseb parseSubBundle)
  , ("U", JD.map Frcub parseUpdateBundle)
  ]
