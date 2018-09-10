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
#-}
module JsonConv () where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(..))

import qualified Data.Set as Set
import Data.Aeson (
    FromJSON(..), ToJSON(..), Value, withArray, Value(..),
    withObject, (.:), object, (.=))
import Data.Aeson.Types (Parser)
import Data.Maybe (fromJust)
import qualified Data.Vector as Vec
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Proxy
import qualified Data.ByteString as BS
import Data.Word
import Data.Int

import Blaze.ByteString.Builder (toByteString)
import Data.Attoparsec.ByteString (parseOnly, endOfInput)

import Clapi.Types.Path (Path, Seg, unSeg, segP, Namespace, Placeholder)
import qualified Clapi.Types.Path as Path
import Clapi.Types.WireTH (mkWithWtProxy)
import Clapi.TaggedData
import Clapi.Serialisation
  ( subMsgTaggedData, SubMsgType(..), interpolationTaggedData
  , dumtTaggedData, DataUpdateMsgType(..)
  , tpcumTaggedData, TpcumType(..)
  , tccumTaggedData, TccumType(..)
  , dataErrIndexTaggedData, subErrIndexTaggedData
  , trBundleTaggedData, TrBundleType(..)
  , Encodable(..)
  , defMsgTaggedData, ilTaggedData, defTaggedData)
import Clapi.Types
  ( Time(..), Interpolation(..), InterpolationType(..), InterpolationLimit(..)
  , DataUpdateMessage(..)
  , TimeStamped(..)
  , FromRelayClientRootBundle(..), FromRelayClientSubBundle(..)
  , FromRelayClientUpdateBundle(..)
  , DataErrorIndex(..), DataErrorMessage(..)
  , SubMessage(..), SubErrorIndex(..), SubErrorMessage(..)
  , ToRelayBundle(..)
  , ToRelayClientUpdateBundle(..), ToRelayClientSubBundle(..)
  , ToProviderContainerUpdateMessage(..), ToClientContainerUpdateMessage(..)
  , TypeMessage(..), Editable(..)
  , WireValue(..)
  , Wireable, WireType(..), wireValueWireType, Tag, mkTag, unTag, (<|$|>)
  , DefMessage(..), Definition(..)
  , ArrayDefinition(..), StructDefinition(..), TupleDefinition(..)
  , PostDefinition(..)
  , unAssocList, TreeType)
import Clapi.TextSerialisation (ttToText)
import Clapi.Util (proxyF, proxyF3)

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

mkWithWtProxy "withJsonWtProxy" [''Wireable, ''ToJSON, ''FromJSON]

instance FromJSON WireType where
    parseJSON v = parseJSON v >>= wtFromString
      where
        -- FIXME: Much the same as a test helper in Clapi
        wtFromString =
          either fail return . parseOnly (parser <* endOfInput) . encodeUtf8

instance ToJSON WireType where
    toJSON = toJSON . wtToText
      where
        wtToText = either error (decodeUtf8 . toByteString) . builder

instance FromJSON WireValue where
    parseJSON = withObject "WireValue" $ \o -> do
        t <- o .: "type"
        v <- o .: "val"
        withJsonWtProxy t go v
      where
        go :: forall a. (Wireable a, FromJSON a) => Proxy a -> Value -> Parser WireValue
        go _ = fmap WireValue . parseJSON @a

instance ToJSON WireValue where
    -- This isn't a trick you can pull here, could do it before because a was Encodable
    toJSON wv =
      let
        wt = wireValueWireType wv
        wvj :: forall a. (Wireable a, ToJSON a) => Proxy a -> Value
        wvj _ = fromJust $ toJSON @a <|$|> wv
        wtjv = withJsonWtProxy wt wvj
      in object ["type" .= toJSON wt, "val" .= wtjv]

instance FromJSON SubMessage where
    parseJSON = parseTaggedJson subMsgTaggedData $ \e -> case e of
        SubMsgTSub -> fmap MsgSubscribe . parseJSON
        SubMsgTTypeSub -> fmap MsgTypeSubscribe . parseJSON
        SubMsgTUnsub -> fmap MsgUnsubscribe . parseJSON
        SubMsgTTypeUnsub -> fmap MsgTypeUnsubscribe . parseJSON

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
    parseJSON = parseTaggedJson interpolationTaggedData $ \e v -> case e of
        ItConstant -> return IConstant
        ItLinear -> return ILinear

instance ToJSON Interpolation where
    toJSON = buildTaggedJson interpolationTaggedData $ const $ toJSON (Nothing :: Maybe Int)

instance FromJSON DataUpdateMessage where
    parseJSON = parseTaggedJson dumtTaggedData $ \e -> withObject "DataUpdateMessage" $ case e of
        DUMTConstSet -> \v -> MsgConstSet
            <$> v .: "path" <*> v .: "args" <*> v .: "att"
        DUMTSet -> \v -> MsgSet
            <$> v .: "path" <*> v .: "tpid" <*> v .: "time" <*> v .: "args"
            <*> v .: "interpolation" <*> v .: "att"
        DUMTRemove -> \v -> MsgRemove
            <$> v .: "path" <*> v .: "tpid" <*> v .: "att"

instance ToJSON DataUpdateMessage where
    toJSON = buildTaggedJson dumtTaggedData $ \dum -> case dum of
        MsgConstSet p vs ma -> object ["path" .= p, "args" .= vs, "att" .= ma]
        MsgSet p tpId t vs i ma -> object
          [ "path" .= p, "tpid" .= tpId, "time" .= t, "args" .= vs
          , "interpolation" .= i, "att" .= ma]
        MsgRemove p tpId ma -> object ["path" .= p, "tpid" .= tpId, "att" .= ma]

instance FromJSON ToProviderContainerUpdateMessage where
  parseJSON = parseTaggedJson tpcumTaggedData $ \case
    TpcumtCreateAfter -> withObject "TpcumCreateAfter" $ \o ->
      TpcumCreateAfter <$> o .: "args" <*> o .: "ph" <*> o .: "ref"
        <*> o .: "att"
    TpcumtMoveAfter -> withObject "TpcumMoveAfter" $ \o ->
      TpcumMoveAfter <$> o .: "tgt" <*> o .: "ref" <*> o .: "att"
    TpcumtAbsent -> withObject "TpcumAbsent" $ \o ->
      TpcumAbsent <$> o .: "tgt" <*> o .: "att"

instance ToJSON ToProviderContainerUpdateMessage where
  toJSON = buildTaggedJson tpcumTaggedData $ \case
    TpcumCreateAfter args ph ref att -> object
      ["args" .= args, "ph" .= ph, "ref" .= ref, "att" .= att]
    TpcumMoveAfter tgt ref att -> object
      ["tgt" .= tgt, "ref" .= ref, "att" .= att]
    TpcumAbsent tgt att -> object ["tgt" .= tgt, "att" .= att]

instance FromJSON ToClientContainerUpdateMessage where
  parseJSON = parseTaggedJson tccumTaggedData $ \case
    TccumtPresentAfter -> withObject "TccumPresentAfter" $ \o ->
      TccumPresentAfter <$> o .: "tgt" <*> o .: "ref" <*> o .: "att"
    TccumtAbsent -> withObject "TccumAbsent" $ \o ->
      TccumAbsent <$> o .: "tgt" <*> o .: "att"

instance ToJSON ToClientContainerUpdateMessage where
  toJSON = buildTaggedJson tccumTaggedData $ \case
    TccumPresentAfter tgt ref att -> object
      ["tgt" .= tgt, "ref" .= ref, "att" .= att]
    TccumAbsent tgt att -> object ["tgt" .= tgt, "att" .= att]

instance FromJSON ToRelayClientUpdateBundle where
    parseJSON = withObject "ToRelayClientUpdateBundle" $ \b ->
      ToRelayClientUpdateBundle <$> b.: "ns" <*> b .: "data" <*> b .: "cont"

instance FromJSON ToRelayClientSubBundle where
    parseJSON = withObject "ToRelayClientSubBundle" $ \b ->
      ToRelayClientSubBundle <$> b .: "subMsgs"

instance (FromJSON a) => FromJSON (TimeStamped a) where
    parseJSON o = withObject "TimeStamped" (\ts -> curry TimeStamped <$> ts .: "time" <*> parseJSON o) o

instance ToJSON DataErrorIndex where
    toJSON = buildTaggedJson dataErrIndexTaggedData $ \case
        GlobalError -> toJSON (Nothing :: Maybe Int)
        PathError p -> toJSON p
        TimePointError p tpid -> object ["path" .= p, "tpid" .= tpid]

instance ToJSON Editable where
    toJSON l = toJSON $ case l of
        Editable -> ("rw" :: String)
        ReadOnly -> "ro"

instance ToJSON DataErrorMessage where
    toJSON (MsgDataError ei m) = object ["eIdx" .= ei, "msg" .= m]

instance ToJSON SubErrorIndex where
    toJSON = buildTaggedJson subErrIndexTaggedData $ \case
        NamespaceSubError ns -> toJSON ns
        PostTypeSubError ns tn -> object ["ns" .= ns, "tn" .= tn]
        TypeSubError ns tn -> object ["ns" .= ns, "tn" .= tn]
        PathSubError ns p -> object ["ns" .= ns, "path" .= p]

instance ToJSON SubErrorMessage where
    toJSON (MsgSubError ei m) = object ["eIdx" .= ei, "msg" .= m]

instance ToJSON TypeMessage where
    toJSON (MsgAssignType p tn l) = object ["path" .= p, "typeName" .= tn, "ed" .= l]

instance ToJSON StructDefinition where
    toJSON (StructDefinition doc al) = object ["doc" .= doc, "stls" .= (asStl <$> unAssocList al)]
      where
        asStl (s, (tn, l)) = object ["seg" .= s, "tn" .= tn, "ed" .= l]

instance ToJSON InterpolationLimit where
    toJSON = buildTaggedJson ilTaggedData $ const $ toJSON (Nothing :: Maybe Text)

instance ToJSON TreeType where
    toJSON = toJSON . ttToText

instance ToJSON TupleDefinition where
    toJSON (TupleDefinition doc types interpLim) = object
        ["doc" .= doc, "types" .= typeOs, "il" .= interpLim]
      where
        typeOs = asTypeO <$> unAssocList types
        asTypeO (s, tt) = object ["seg" .= s, "ty" .= tt]

instance ToJSON PostDefinition where
    toJSON (PostDefinition doc args) = object ["doc" .= doc, "args" .= typeOs]
      where
        typeOs = asTypeO <$> unAssocList args
        asTypeO (s, tts) = object ["seg" .= s, "tys" .= toJSONList tts]

instance ToJSON ArrayDefinition where
    toJSON (ArrayDefinition doc ptn cTn cLib) =
      object ["doc" .= doc, "ptn" .= ptn, "ctn" .= cTn, "clib" .= cLib]

instance ToJSON Definition where
    toJSON = buildTaggedJson defTaggedData $ \v -> case v of
        TupleDef d -> toJSON d
        StructDef d -> toJSON d
        ArrayDef d -> toJSON d

instance (ToJSON ident, ToJSON def) => ToJSON (DefMessage ident def) where
    toJSON = buildTaggedJson defMsgTaggedData $ \v -> case v of
        MsgDefine i def -> object ["id" .= toJSON i, "def" .= toJSON def]
        MsgUndefine i -> toJSON i

instance ToJSON FromRelayClientUpdateBundle where
    toJSON (FromRelayClientUpdateBundle ns errs pdefs defs tas dd co) = object
        [ "ns" .= toJSON ns
        , "errs" .= toJSONList errs
        , "pdefs" .= toJSONList pdefs
        , "defs" .= toJSONList defs
        , "tas" .= toJSONList tas
        , "dd" .= toJSONList dd
        , "co" .= toJSONList co
        ]

instance ToJSON FromRelayClientRootBundle where
    toJSON (FromRelayClientRootBundle co) = object ["co" .= toJSONList co]

instance ToJSON FromRelayClientSubBundle where
    toJSON (FromRelayClientSubBundle errs ptuns tuns duns) = object
      [ "errs" .= toJSON errs
      , "ptuns" .= toJSON ptuns
      , "tuns" .= toJSON tuns
      , "duns" .= toJSON duns
      ]

instance FromJSON ToRelayBundle where
    parseJSON = parseTaggedJson trBundleTaggedData $ \case
      TrbtClientSub -> fmap Trcsb . parseJSON
      TrbtClientUpdate -> fmap Trcub . parseJSON
      -- FIXME: there might be a better way to handle this condition:
      _ -> error "Web client acted as provider"
