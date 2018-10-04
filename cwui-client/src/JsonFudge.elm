module JsonFudge exposing (serialiseDigest, parseDigest)

import Json.Encode as JE
import Json.Decode as JD
import Dict exposing (Dict)

import Futility exposing (Either(..))
import Tagged.Tagged exposing (Tagged(..))
import ClTypes exposing
  ( Namespace, Placeholder, Path, Time, Interpolation(..), TpId, Seg
  , Attributee , TypeName, typeName, Editable(..), Definition(..)
  , PostDefinition, AtomDef , InterpolationLimit(..) , ChildDescription
  , WireValue(..), WireType(..), SubPath, typeNameGetNs, typeNameGetSeg
  , SubErrorIndex(..), DataErrorIndex(..))
import ClSpecParser exposing (parseAtomDef)
import Cmp.Set as CSet
import Cmp.Dict as CDict exposing (CmpDict)
import Digests exposing
  ( Digest, DefOp(..), DataDigest, DataChange(..), ConstChangeT
  , TimeSeriesDataOp(..), NsDigest, Cops, TrcUpdateDigest, CreateOp, TrcSubDigest, SubOp(..), ToRelayDigest(..))
import SequenceOps exposing (SeqOp(..))
import Tagged.Dict as TD exposing (TaggedDict)
import Tagged.Set as TS

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

encodeSubPath : SubPath -> JE.Value
encodeSubPath (Tagged p) = encodePair encodeSeg encodePath p

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

encodeAttributee : Maybe Attributee -> JE.Value
encodeAttributee = encodeNullable JE.string

encodeInterpolation i = case i of
    IConstant -> tagged 'C' (JE.list [])
    ILinear -> tagged 'L' (JE.list [])

encodeDict : (comparable -> JE.Value) -> (v -> JE.Value) -> Dict comparable v -> JE.Value
encodeDict ek ev d = JE.list <| List.map (encodePair ek ev) <| Dict.toList d

encodeCDict : (k -> JE.Value) -> (v -> JE.Value) -> CmpDict k comparable v -> JE.Value
encodeCDict ek ev d = JE.list <| List.map (encodePair ek ev) <| CDict.toList d

encodeTsdo : TimeSeriesDataOp -> JE.Value
encodeTsdo tsdo = case tsdo of
    OpSet t wts wvs i -> tagged 's' <| JE.object
      [ ("time", encodeTime t)
      , ("wvs", JE.list <| List.map2 encodeWv wts wvs)
      , ("interp", encodeInterpolation i)
      ]
    OpRemove -> tagged 'r' JE.null

encodeDataChange : DataChange -> JE.Value
encodeDataChange dc = case dc of
    ConstChange (ma, wts, wvs) -> tagged 'c' <| JE.object
      [ ("att", encodeAttributee ma)
      , ("wvs", JE.list <| List.map2 encodeWv wts wvs)
      ]
    TimeChange tc -> tagged 't' <| encodeDict JE.int (encodePair encodeAttributee encodeTsdo) tc
    DeletedChange -> JE.string "somehow got a deleted"

encodeCreateOp : CreateOp -> JE.Value
encodeCreateOp {args, after} = JE.object
  [ ("args", JE.list <| List.map (JE.list << List.map (uncurry encodeWv)) args)
  , ("after", encodeNullable (encodeEither encodePlaceholder encodeSeg) after)
  ]

encodeCreates : CmpDict Placeholder Seg (Maybe Attributee, CreateOp) -> JE.Value
encodeCreates = encodeCDict encodePlaceholder (encodePair encodeAttributee encodeCreateOp)

encodeSeqOp : SeqOp (Either Placeholder Seg) -> JE.Value
encodeSeqOp op = case op of
    SoPresentAfter ref -> tagged '>' <| encodeNullable (encodeEither encodePlaceholder encodeSeg) ref
    SoAbsent -> tagged 'x' JE.null

encodeCops : Cops (Either Placeholder Seg) -> JE.Value
encodeCops = encodeDict encodeSeg <| encodePair encodeAttributee <| encodeSeqOp

encodeTrcud : TrcUpdateDigest -> JE.Value
encodeTrcud trcud = JE.object
  [ ("ns", encodeNs <| .ns trcud)
  , ("dd", encodeDict encodePath encodeDataChange <| .dd trcud)
  , ("creates", encodeDict encodePath encodeCreates <| .creates trcud)
  , ("co", encodeDict encodePath encodeCops <| .co trcud)
  ]

encodeSubOp : SubOp -> JE.Value
encodeSubOp o = case o of
    Subscribe -> JE.list [JE.string "s", JE.null]
    Unsubscribe -> JE.list [JE.string "u", JE.null]

encodeTypePtr : (Namespace, Tagged a Seg) -> JE.Value
encodeTypePtr = encodePair encodeNs encodeTaggedSeg

encodeTrcsd : TrcSubDigest -> JE.Value
encodeTrcsd trcsd = JE.object
  [ ("postTypes", encodeCDict encodeTypePtr encodeSubOp <| .postTypes trcsd)
  , ("types", encodeCDict encodeTypePtr encodeSubOp <| .types trcsd)
  , ("data", encodeCDict encodeSubPath encodeSubOp <| .data trcsd)
  ]

serialiseDigest : ToRelayDigest -> Time -> String
serialiseDigest b t = JE.encode 2 <| JE.object
  [ ("time", encodeTime t)
  , ("val", case b of
        Trcud ub -> tagged 'u' <| encodeTrcud ub
        Trcsd sb -> tagged 's' <| encodeTrcsd sb)
  ]

-- Decoding:

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

decodeDict : JD.Decoder comparable -> JD.Decoder v -> JD.Decoder (Dict comparable v)
decodeDict dk dv = JD.map Dict.fromList <| JD.list <| decodePair dk dv

decodeTaOps : JD.Decoder (Dict Path (Tagged Definition Seg, Editable))
decodeTaOps = decodeDict decodePath <|
    decodePair (JD.map Tagged decodeSeg) decodeEditable

decodeDefOp : JD.Decoder a -> JD.Decoder (DefOp a)
decodeDefOp defDec = decodeTagged <| Dict.fromList
  [ ("d", JD.map OpDefine defDec)
  , ("u", JD.succeed OpUndefine)
  ]

decodeDefs : JD.Decoder a -> JD.Decoder (TaggedDict a Seg (DefOp a))
decodeDefs defDec = JD.map TD.fromList <| JD.list <| decodePair
    (JD.map Tagged decodeSeg) <| decodeDefOp defDec

decodeSubDigest : JD.Decoder Digest
decodeSubDigest = JD.map4 mungeFrsed
    (JD.field "errs" (JD.list <| decodePair decodeSubErrIdx <| JD.list JD.string))
    (JD.field "ptuns" (JD.list decodeTypeName))
    (JD.field "tuns" (JD.list decodeTypeName))
    (JD.field "duns" (JD.list decodeSubPath))

mungeFrsed
   : List (SubErrorIndex, List String) -> List (Tagged PostDefinition TypeName)
  -> List (Tagged Definition TypeName) -> List SubPath -> Digest
mungeFrsed errs pUnsubs tUnsubs dUnsubs =
  let
    insertUndef ts = CDict.insert ts OpUndefine
    insertUnsub p = Dict.insert p DeletedChange
    nsOrient = List.foldl
        (\tn -> CDict.update
            (typeNameGetNs tn)
            (Just << (\a -> typeNameGetSeg tn :: a) << Maybe.withDefault []))
        TD.empty
    nsoPuns = nsOrient pUnsubs
    nsoTuns = nsOrient tUnsubs
    nsoDuns = List.foldl
        (\(Tagged (ns, p)) -> CDict.update (Tagged ns) (Just << (\a -> p :: a) << Maybe.withDefault []))
        TD.empty dUnsubs
    mentionedNss = TS.fromList <| CDict.keys nsoPuns ++ CDict.keys nsoTuns ++ CDict.keys nsoDuns
    forNs ns conv nsoUnsubs = case CDict.get ns nsoUnsubs of
        Nothing -> TD.empty
        Just unsubs -> List.foldl conv TD.empty unsubs
    genNsd ns = CDict.insert ns
      { postDefs = forNs ns insertUndef nsoPuns
      , defs = forNs ns insertUndef nsoTuns
      , dops = case CDict.get ns nsoDuns of
            Nothing -> Dict.empty
            Just unsubs -> List.foldl insertUnsub Dict.empty unsubs
      , taOps = Dict.empty
      , cops = Dict.empty
      , errs = []
      }
    nsds = CSet.foldl genNsd TD.empty mentionedNss
  in {rootCops = Dict.empty, nsds = nsds, subErrs = errs}

decodeErr : JD.Decoder (DataErrorIndex, List String)
decodeErr = decodePair decodeDataErrIdx (JD.list JD.string)

decodeSeqOp : JD.Decoder (SeqOp Seg)
decodeSeqOp = decodeTagged <| Dict.fromList
  [ (">", JD.map SoPresentAfter <| JD.nullable decodeSeg)
  , ("x", JD.succeed SoAbsent)
  ]

decodeContOps : JD.Decoder (Cops Seg)
decodeContOps = decodeDict decodeSeg <|
    decodePair (JD.nullable decodeAttributee) decodeSeqOp

decodeConstChange : JD.Decoder ConstChangeT
decodeConstChange = JD.map2 (\atts (wts, wvs) -> (atts, wts, wvs))
    (JD.field "att" <| JD.nullable decodeAttributee)
    (JD.field "wvs" <| JD.map List.unzip <| JD.list decodeWv)

decodeTsdo : JD.Decoder TimeSeriesDataOp
decodeTsdo = decodeTagged <| Dict.fromList <|
  [ ("s", JD.map3 (\t (wts, wvs) i -> OpSet t wts wvs i)
        (JD.field "time" decodeTime)
        (JD.map List.unzip <| JD.field "wvs" <| JD.list decodeWv)
        (JD.field "interp" decodeInterpolation))
  , ("r", JD.succeed OpRemove)
  ]

decodeDataChange : JD.Decoder DataChange
decodeDataChange = decodeTagged <| Dict.fromList
  [ ("c", JD.map ConstChange decodeConstChange)
  , ("t", JD.map TimeChange <| decodeDict
        decodeTpId
        <| JD.map2 (,) (JD.nullable decodeAttributee) decodeTsdo)
  ]

decodeData : JD.Decoder DataDigest
decodeData = decodeDict decodePath decodeDataChange

decodeNsd : JD.Decoder NsDigest
decodeNsd = JD.map6
    (\pd d ta co do e ->
      {postDefs = pd, defs = d, taOps = ta, cops = co, dops = do, errs = e})
    (JD.field "pdefs" <| decodeDefs decodePostDef)
    (JD.field "defs" <| decodeDefs decodeDef)
    (JD.field "tas" decodeTaOps)
    (JD.field "co" <| decodeDict decodePath decodeContOps)
    (JD.field "dd" decodeData)
    (JD.field "errs" <| JD.list decodeErr)

parseDigest : String -> Result String Digest
parseDigest = JD.decodeString <| decodeTagged <| Dict.fromList
  [ ("r", JD.map
        (\rcs -> {nsds = TD.empty, rootCops = rcs, subErrs = []})
        (decodeDict decodeSeg decodeSeqOp))
  , ("s", decodeSubDigest)
  , ("u", JD.map2
        (\ns nsd -> {nsds = TD.singleton ns nsd, rootCops = Dict.empty, subErrs = []})
        (JD.field "ns" <| decodeNs)
        decodeNsd)
  ]
