module JsonFudge exposing (serialiseBundle, parseBundle)

import Json.Encode as JE
import Json.Decode as JD
import Dict

import Futility exposing (Either(..))
import Tagged.Tagged exposing (Tagged(..))
import ClTypes exposing
  ( Namespace, Placeholder, Path, Time, Interpolation(..), TpId, Seg
  , Attributee , TypeName, typeName, Editable(..), Definition(..)
  , PostDefinition, AtomDef , InterpolationLimit(..) , ChildDescription
  , WireValue(..), WireType(..), SubPath)
import ClSpecParser exposing (parseAtomDef)
import ClMsgTypes exposing (..)

-- Json serialisation fudging

encodePair : (a -> JE.Value) -> (b -> JE.Value) -> (a, b) -> JE.Value
encodePair ea eb (a, b) = JE.list [ ea a, eb b ]

encodeEither : (a -> JE.Value) -> (b -> JE.Value) -> Either a b -> JE.Value
encodeEither ea eb e = JE.object <| case e of
    Left a -> [("Left", ea a)]
    Right b -> [("Right", eb b)]

encodeNullable : (a -> JE.Value) -> Maybe a -> JE.Value
encodeNullable encA ma = case ma of
    Just a -> encA a
    Nothing -> JE.null

encodeSeg : Seg -> JE.Value
encodeSeg = JE.string

encodeTaggedSeg : Tagged a Seg -> JE.Value
encodeTaggedSeg (Tagged s) = JE.string s

encodePlaceholder : Placeholder -> JE.Value
encodePlaceholder = encodeTaggedSeg

encodeNs : Namespace -> JE.Value
encodeNs = encodeTaggedSeg

encodePath : Path -> JE.Value
encodePath = JE.string

encodeTypeName : Tagged a TypeName -> JE.Value
encodeTypeName (Tagged (ns, seg)) = JE.object
  [ ("ns", encodeSeg ns)
  , ("seg", encodeSeg seg)]

encodeSubPath : SubPath -> JE.Value
encodeSubPath (Tagged p) = encodePair encodeSeg encodePath p

encodeSubMsg : SubMsg -> JE.Value
encodeSubMsg sm = JE.list <| case sm of
    MsgSub p -> [JE.string "S", encodePath p]
    MsgTypeSub tn -> [JE.string "T", encodeTaggedSeg tn]
    MsgPostTypeSub tn -> [JE.string "P", encodeTaggedSeg tn]
    MsgUnsub p -> [JE.string "s", encodePath p]
    MsgTypeUnsub tn -> [JE.string "t", encodeTaggedSeg tn]
    MsgPostTypeUnsub tn -> [JE.string "p", encodeTaggedSeg tn]

encodeTime : Time -> JE.Value
encodeTime (s, f) = JE.list [JE.int s, JE.int f]

tagged : Char -> JE.Value -> JE.Value
tagged t v = JE.list [JE.string (String.fromChar t), v]

encodeWv : WireType -> WireValue -> JE.Value
encodeWv wt wv =
  let
    encodeWv v = case v of
        (WvTime t) -> encodeTime t
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

providerContToJsonValue : ToProviderContainerUpdateMsg -> JE.Value
providerContToJsonValue m = case m of
    MsgCreateAfter {msgPostArgs, msgTgt, msgRef, msgAttributee} -> tagged '+' <| JE.object
      [ ("args", JE.list <| List.map (JE.list << List.map (uncurry encodeWv)) msgPostArgs)
      , ("tgt", encodePlaceholder msgTgt)
      , ("ref", encodeNullable (encodeEither encodePlaceholder encodeSeg) msgRef)
      , encodeAttributeeField msgAttributee
      ]
    MsgMoveAfter {msgTgt, msgRef, msgAttributee} -> tagged '>' <| JE.object
      [ encodeTgtField msgTgt
      , encodeRefField msgRef
      , encodeAttributeeField msgAttributee
      ]
    MsgDelete {msgTgt, msgAttributee} -> tagged '-' <| JE.object
      [ encodeTgtField msgTgt
      , encodeAttributeeField msgAttributee
      ]

encodeUpdateBundle : ToRelayUpdateBundle -> Time -> JE.Value
encodeUpdateBundle (ToRelayUpdateBundle ns dums conts) t = JE.object
  [ ("ns", encodeNs ns)
  , ("data", JE.list (List.map dumToJsonValue dums))
  , ("cont", JE.list (List.map (encodePair encodePath providerContToJsonValue) conts))
  ]

encodeSubBundle : ToRelaySubBundle -> Time -> JE.Value
encodeSubBundle (ToRelaySubBundle subs) t = JE.object
  [ ("subMsgs", JE.list (List.map (encodePair encodeNs encodeSubMsg) subs)) ]

serialiseBundle : ToRelayClientBundle -> Time -> String
serialiseBundle b t = JE.encode 2 <| JE.object
  [ ("time", encodeTime t)
  , ("val", case b of
        Trcub ub -> tagged 'U' <| encodeUpdateBundle ub t
        Trcsb sb -> tagged 'S' <| encodeSubBundle sb t)
  ]

decodePair : JD.Decoder a -> JD.Decoder b -> JD.Decoder (a, b)
decodePair da db = JD.map2 (,) (JD.field "0" da) (JD.field "1" db)

decodePath : JD.Decoder String
decodePath = JD.string

decodeDataErrIdx : JD.Decoder DataErrorIndex
decodeDataErrIdx = decodeTagged (Dict.fromList
    [ ("g", JD.succeed DGlobalError)
    , ("p", JD.map DPathError decodePath)
    , ("t", JD.map2 DTimePointError decodePath decodeTpId)
    ])

decodeSubPath : JD.Decoder SubPath
decodeSubPath = JD.map Tagged <| decodePair decodeSeg decodePath

decodeSubErrIdx : JD.Decoder SubErrorIndex
decodeSubErrIdx = decodeTagged (Dict.fromList
    [ ("p", JD.map SPathError decodeSubPath)
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
decodeChildDesc = JD.map3 (\n t e -> {name = n, typeRef = t, ed = e})
    (JD.field "seg" decodeSeg)
    (JD.field "tn" decodeSeg)
    (JD.field "ed" decodeEditable)

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
  , ("A", JD.map4
      (\d ptn ctn ctl -> ArrayDef {doc = d, postType = ptn, childType = ctn, childEditable = ctl})
      decodeDocField
      (JD.field "ptn" <| JD.nullable <| JD.map Tagged decodeSeg)
      (JD.field "ctn" decodeSeg)
      (JD.field "ced" decodeEditable))
  ]

decodeDef : JD.Decoder Definition
decodeDef = decodeTagged defTagDecoders

decodePostDef : JD.Decoder PostDefinition
decodePostDef = JD.map2 PostDefinition
    (JD.field "doc" JD.string)
    (JD.field "args" <| JD.list <| JD.map2 (,)
        (JD.field "seg" JD.string)
        (JD.field "tys" <| JD.list decodeAtomDef))

decodeDefMsg : JD.Decoder a -> JD.Decoder (DefMsg a)
decodeDefMsg defDec = decodeTagged (Dict.fromList
  [ ("d", JD.map2 MsgDefine (JD.field "id" <| JD.map Tagged decodeSeg) (JD.field "def" defDec))
  , ("u", JD.map MsgUndefine <| JD.map Tagged decodeSeg)
  ])

decodeEditable : JD.Decoder Editable
decodeEditable =
  let
    edFromString s = case s of
        "ro" -> JD.succeed ReadOnly
        "rw" -> JD.succeed Editable
        _ -> JD.fail ("Unrecognised editable string: " ++ s)
  in JD.andThen edFromString JD.string

decodeWv : JD.Decoder (WireType, WireValue)
decodeWv =
  let
    unpackWt wts = case String.uncons wts of
        Just (awt, rem) -> case awt of
            't' -> Ok WtTime
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

decodeNs : JD.Decoder Namespace
decodeNs = JD.map Tagged decodeSeg

decodeTypeName : JD.Decoder (Tagged a TypeName)
decodeTypeName = JD.map2 typeName
    (JD.field "0" decodeNs) (JD.field "1" <| JD.map Tagged decodeSeg)

decodeTypeMsg : JD.Decoder TypeMsg
decodeTypeMsg = JD.map3 MsgAssignType
    decodePathField
    (JD.field "typeName" <| JD.map Tagged decodeSeg)
    (JD.field "ed" decodeEditable)

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
    [ (">", JD.map3
          (\t r a -> MsgPresentAfter
            {msgTgt = t, msgRef = r, msgAttributee = a})
          decodeTgtField
          (JD.field "ref" (JD.nullable decodeSeg))
          decodeAttributeeField)
    , ("-", JD.map2
          (\t a -> MsgAbsent
            {msgTgt = t, msgAttributee = a})
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
    (JD.field "duns" (JD.list decodeSubPath))

parseUpdateBundle : JD.Decoder FromRelayClientUpdateBundle
parseUpdateBundle = JD.map7 FromRelayClientUpdateBundle
    (JD.field "ns" <| JD.map Tagged JD.string)
    (JD.field "errs" (JD.list <| decodeErrMsg decodeDataErrIdx))
    (JD.field "pdefs" (JD.list <| decodeDefMsg decodePostDef))
    (JD.field "defs" (JD.list <| decodeDefMsg decodeDef))
    (JD.field "tas" (JD.list decodeTypeMsg))
    (JD.field "dd" (JD.list decodeDum))
    (JD.field "co" (JD.list <| decodePair decodePath decodeCCm))

parseBundle : String -> Result String FromRelayClientBundle
parseBundle = JD.decodeString <| decodeTagged <| Dict.fromList
  [ ("R", JD.map Frrub parseRootBundle)
  , ("S", JD.map Frseb parseSubBundle)
  , ("U", JD.map Frcub parseUpdateBundle)
  ]
