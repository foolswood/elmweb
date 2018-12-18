{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE
    FlexibleInstances
  , GeneralizedNewtypeDeriving
  , LambdaCase
  , OverloadedStrings
  , Rank2Types
  , ScopedTypeVariables
  , StandaloneDeriving
  , TemplateHaskell
  , TypeApplications
  , TypeSynonymInstances
  , GADTs
#-}
module JsonConv () where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(..))

import Language.Haskell.TH (Type(ConT))
import Data.Constraint (Dict(..))
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Aeson (
    FromJSON(..), ToJSON(..), Value, withArray, Value(..),
    withObject, (.:), object, (.=))
import Data.Aeson.Types (Parser)
import qualified Data.Vector as Vec
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.ByteString as BS

import Blaze.ByteString.Builder (toByteString)
import Data.Attoparsec.ByteString (parseOnly, endOfInput)

import Data.Map.Mos as Mos
import qualified Data.Map.Mol as Mol
import Clapi.Types.Path (Path, Seg, unSeg, segP, Namespace, Placeholder)
import qualified Clapi.Types.Path as Path
import Clapi.Types.WireTH (mkGetWtConstraint)
import Clapi.TaggedData
import Clapi.Serialisation
  ( interpolationTaggedData , dataErrIndexTaggedData, subErrIndexTaggedData
  , Encodable(..) , ilTaggedData, defTaggedData, frTaggedData, trTaggedData
  , soTaggedData, dcTaggedData, tsDOpTaggedData, defOpTaggedData
  , subOpTaggedData, TrDigestType(..), SeqOpType(..), DataChangeType(..)
  , TsDOpT(..), parser)
import Clapi.Types
  ( Time(..), Interpolation(..), InterpolationType(..), InterpolationLimit(..)
  , TimeStamped(..), DataErrorIndex(..), SubErrorIndex(..), Editability(..)
  , Attributee(..), WireValue(..), SomeWireValue(..), WireType(..)
  , SomeWireType(..), someWv, Tag, mkTag, unTag, Definition(..)
  , SomeDefinition(..), PostDefinition(..)
  , unAssocList, alFromList, SomeTreeType, FrDigest(..), TrDigest(..)
  , FrcRootDigest(..), FrcSubDigest(..) , FrcUpdateDigest(..), DataChange(..)
  , TimeSeriesDataOp(..), DefOp(..), SubOp(..), CreateOp(..), TrcSubDigest(..)
  , TrcUpdateDigest(..), DataDigest, withWireValue, withWireType)
import Clapi.Types.SequenceOps (SequenceOp(..))
import Clapi.TextSerialisation (ttToText_)

parseTaggedJson :: TaggedData e a -> (e -> Value -> Parser a) -> Value -> Parser a
parseTaggedJson td p = withArray "Tagged" (handleTagged . Vec.toList)
  where
    tagSet = Set.fromList $ tdAllTags td
    handleTagged [jt, v] = do
        ts <- parseJSON jt
        t <- bsAsTag ts
        if t `elem` tagSet
            then p (tdTagToEnum td t) v
            else fail $ "Bad tag '" ++ (T.unpack ts) ++ "' expecting '" ++ (show $ Set.toList tagSet) ++ "'"
    handleTagged _ = fail "Invalid tagged value"

bsAsTag :: MonadFail m => Text -> m Tag
bsAsTag t = case T.length t of
    1 -> mkTag $ BS.head $ encodeUtf8 t
    _ -> fail "Bad tag string"

instance FromJSON Tag where
    parseJSON v = parseJSON v >>= bsAsTag

instance ToJSON Tag where
    toJSON = toJSON . decodeUtf8 . BS.singleton . unTag

instance FromJSON Path where
    parseJSON v = parseJSON v >>= Path.fromText segP

instance ToJSON Path where
    toJSON = toJSON . Path.toText unSeg

instance FromJSON Seg where
    parseJSON v = parseJSON v >>= Path.mkSeg

instance ToJSON Seg where
    toJSON = toJSON . Path.unSeg

deriving instance ToJSON Namespace
deriving instance FromJSON Namespace

deriving instance ToJSON Placeholder
deriving instance FromJSON Placeholder

deriving instance ToJSON Attributee
deriving instance FromJSON Attributee

mkGetWtConstraint "getWtToJson" $ ConT ''ToJSON
mkGetWtConstraint "getWtFromJson" $ ConT ''FromJSON

instance FromJSON SomeWireType where
    parseJSON v = parseJSON v >>= wtFromString
      where
        -- FIXME: Much the same as a test helper in Clapi
        wtFromString =
          either fail return . parseOnly (parser <* endOfInput) . encodeUtf8

instance ToJSON (WireType a) where
    toJSON = toJSON . wtToText
      where
        wtToText = either error (decodeUtf8 . toByteString) . builder

instance ToJSON SomeWireType where
    toJSON = withWireType toJSON

instance FromJSON SomeWireValue where
    parseJSON = withObject "WireValue" $ \o -> do
        SomeWireType wt <- o .: "type"
        case getWtFromJson wt of
            Dict -> someWv wt <$> o .: "val"

instance ToJSON (WireValue a) where
    toJSON (WireValue wt a) = case getWtToJson wt of
        Dict -> object ["type" .= toJSON wt, "val" .= toJSON a]

instance ToJSON SomeWireValue where
    toJSON = withWireValue toJSON

instance FromJSON Time where
    parseJSON = withArray "Time" (\v -> case Vec.toList v of
        [sv, fv] -> do
            s <- parseJSON sv
            f <- parseJSON fv
            return $ Time s f
        _ -> fail "not of the form [s, f]")

instance ToJSON Time where
    toJSON (Time s f) = toJSON [toJSON s, toJSON f]

buildTaggedJson :: TaggedData e a -> (a -> Value) -> a -> Value
buildTaggedJson td b i = toJSON [toJSON $ tdInstanceToTag td i, b i]

instance FromJSON Interpolation where
    parseJSON = parseTaggedJson interpolationTaggedData $ \e _v -> case e of
        ItConstant -> return IConstant
        ItLinear -> return ILinear
        ItBezier -> error "Unsupported right now"

instance ToJSON Interpolation where
    toJSON = buildTaggedJson interpolationTaggedData $ const $ toJSON (Nothing :: Maybe Int)

instance (FromJSON a) => FromJSON (TimeStamped a) where
    parseJSON = withObject "TimeStamped" $ \ts -> curry TimeStamped <$> ts .: "time" <*> ts .: "val"

instance ToJSON DataErrorIndex where
    toJSON = buildTaggedJson dataErrIndexTaggedData $ \case
        GlobalError -> toJSON (Nothing :: Maybe Int)
        NamespaceError ns -> toJSON ns
        PathError p -> toJSON p
        TimePointError p tpid -> object ["path" .= p, "tpid" .= tpid]

instance ToJSON Editability where
    toJSON l = toJSON $ case l of
        Editable -> ("rw" :: String)
        ReadOnly -> "ro"

instance ToJSON SubErrorIndex where
    toJSON = buildTaggedJson subErrIndexTaggedData $ \case
        NamespaceSubError ns -> toJSON ns
        PostTypeSubError ns tn -> object ["ns" .= ns, "tn" .= tn]
        TypeSubError ns tn -> object ["ns" .= ns, "tn" .= tn]
        PathSubError ns p -> toJSON (ns, p)

instance ToJSON InterpolationLimit where
    toJSON = buildTaggedJson ilTaggedData $ const $ toJSON (Nothing :: Maybe Text)

instance ToJSON SomeTreeType where
    toJSON = toJSON . ttToText_

instance ToJSON PostDefinition where
    toJSON (PostDefinition doc args) = object ["doc" .= doc, "args" .= typeOs]
      where
        typeOs = asTypeO <$> unAssocList args
        asTypeO (s, tts) = object ["seg" .= s, "tys" .= toJSONList tts]

instance ToJSON SomeDefinition where
    toJSON = buildTaggedJson defTaggedData $ \(SomeDefinition d) -> case d of
        TupleDef doc tys ilimit -> object
          [ "doc" .= doc
          , "types" .= (
            (\(s, tt) -> object ["seg" .= s, "ty" .= tt])
            <$> unAssocList tys)
          , "il" .= ilimit
          ]
        StructDef doc al -> object
          [ "doc" .= doc
          , "stls" .= (
            (\(s, (tn, l)) -> object ["seg" .= s, "tn" .= tn, "ed" .= l])
            <$> unAssocList al)
          ]
        ArrayDef doc ptn cTn cEd -> object
          [ "doc" .= doc
          , "ptn" .= ptn
          , "ctn" .= cTn
          , "ced" .= cEd
          ]

instance ToJSON a => ToJSON (SequenceOp a) where
    toJSON = buildTaggedJson soTaggedData $ \o -> case o of
        SoAfter ref -> toJSON ref
        SoAbsent -> Null

instance FromJSON a => FromJSON (SequenceOp a) where
    parseJSON = parseTaggedJson soTaggedData $ \o -> case o of
        SoAfterT -> \ref -> SoAfter <$> parseJSON ref
        SoAbsentT -> const $ pure SoAbsent

instance ToJSON FrcRootDigest where
    toJSON (FrcRootDigest contOps) = toJSON $ Map.toList contOps

instance ToJSON FrcSubDigest where
    toJSON (FrcSubDigest errs ptu tu du) = object
      [ "errs" .= Map.toList errs
      -- FIXME: These are MOS but the elm doesn't know that yet:
      , "ptuns" .= Mos.toList ptu
      , "tuns" .= Mos.toList tu
      , "duns" .= Mos.toList du
      ]

instance ToJSON TimeSeriesDataOp where
    toJSON = buildTaggedJson tsDOpTaggedData $ \o -> case o of
        OpSet t wvs i -> object ["time" .= t , "wvs" .= wvs , "interp" .= i]
        OpRemove -> Null

instance FromJSON TimeSeriesDataOp where
    parseJSON = parseTaggedJson tsDOpTaggedData $ \o -> case o of
        OpSetT -> withObject "OpSet" $ \s ->
            OpSet <$> s .: "time" <*> s .: "wvs" <*> s .: "interp"
        OpRemoveT -> const $ pure OpRemove

instance ToJSON DataChange where
    toJSON = buildTaggedJson dcTaggedData $ \c -> case c of
        ConstChange att wvs -> object ["att" .= att, "wvs" .= wvs]
        TimeChange tc -> toJSON $ Map.toList tc

instance FromJSON DataChange where
    parseJSON = parseTaggedJson dcTaggedData $ \t -> case t of
        ConstChangeT -> withObject "ConstChange" $ \c -> ConstChange <$> c .: "att" <*> c .: "wvs"
        TimeChangeT -> fmap (TimeChange . Map.fromList) . parseJSON

instance ToJSON a => ToJSON (DefOp a) where
    toJSON = buildTaggedJson defOpTaggedData $ \o -> case o of
        OpDefine d -> toJSON d
        OpUndefine -> Null

instance ToJSON FrcUpdateDigest where
    toJSON frcud = object
      [ "ns" .= frcudNamespace frcud
      , "pdefs" .= Map.toList (frcudPostDefs frcud)
      , "defs" .= Map.toList (frcudDefinitions frcud)
      , "tas" .= Map.toList (frcudTypeAssignments frcud)
      , "co" .= Map.toList (Map.toList <$> frcudContOps frcud)
      , "dd" .= unAssocList (frcudData frcud)
      , "errs" .= Mol.toList (frcudErrors frcud)
      ]

instance ToJSON FrDigest where
    toJSON = buildTaggedJson frTaggedData $ \d -> case d of
        Frcrd frcrd -> toJSON frcrd
        Frcsd frcsd -> toJSON frcsd
        Frcud frcud -> toJSON frcud
        Frpd _ -> error "Unexpectedly received provider digest"
        Frped _ -> error "Unexpectedly received provider error digest"

instance FromJSON SubOp where
    parseJSON = parseTaggedJson subOpTaggedData $ const . pure

instance FromJSON DataDigest where
    parseJSON = fmap alFromList . parseJSON

instance FromJSON CreateOp where
    parseJSON = withObject "CreateOp" $ \o -> OpCreate <$> o .: "args" <*> o .: "after"

instance FromJSON TrDigest where
    parseJSON = parseTaggedJson trTaggedData $ \d -> case d of
        TrcsdT -> withObject "SubDigest" $ \sd -> Trcsd <$> (
          TrcSubDigest
          <$> (Map.fromList <$> sd .: "postTypes")
          <*> (Map.fromList <$> sd .: "types")
          <*> (Map.fromList <$> sd .: "data")
          )
        TrcudT -> withObject "UpdateDigest" $ \ud -> Trcud <$> (
          TrcUpdateDigest
          <$> ud .: "ns"
          <*> ud .: "dd"
          <*> (Map.fromList . (fmap $ fmap Map.fromList) <$> ud .: "creates")
          <*> (Map.fromList . (fmap $ fmap Map.fromList) <$> ud .: "co")
          )
        TrprdT -> error "Unexpected provider root digest"
        TrpdT -> error "Unexpected provider digest"
