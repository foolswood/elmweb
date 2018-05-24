{-# LANGUAGE
    OverloadedStrings
  , Rank2Types
  , ScopedTypeVariables
  , TemplateHaskell
  , TypeApplications
#-}
module JsonConv (toRelayClientBundleToClapi, fromRelayClientBundleToJson) where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(..))

import qualified Data.Set as Set
import Data.Aeson (
    FromJSON(..), ToJSON(..), Value, withArray, Value(..),
    withObject, (.:), eitherDecode, object, (.=), encode)
import Data.Aeson.Types (Parser)
import Data.Maybe (fromJust)
import qualified Data.Vector as Vec
import qualified Data.ByteString.Lazy as B
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Proxy
import qualified Data.ByteString as BS
import Data.Word
import Data.Int

import Blaze.ByteString.Builder (toByteString)
import Data.Attoparsec.ByteString (parseOnly, endOfInput)

import Clapi.Types.Path (Path, Seg)
import qualified Clapi.Types.Path as Path
import Clapi.Types.WireTH (mkWithWtProxy)
import Clapi.TaggedData
import Clapi.Serialisation
  ( subMsgTaggedData, SubMsgType(..), interpolationTaggedData
  , dumtTaggedData, DataUpdateMsgType(..), cumtTaggedData
  , ContainerUpdateMsgType(..), errIdxTaggedData
  , Encodable(..)
  , defMsgTaggedData, ilTaggedData, defTaggedData)
import Clapi.Types
  ( Time(..), Interpolation(..), InterpolationType(..), InterpolationLimit(..)
  , DataUpdateMessage(..)
  , TimeStamped(..), FromRelayClientBundle(..), MsgError(..)
  , ToRelayClientBundle(..), SubMessage(..), ContainerUpdateMessage(..)
  , TypeMessage(..), TypeName(..), Liberty(..), ErrorIndex(..), WireValue(..)
  , Wireable, WireType(..), wireValueWireType, Tag, mkTag, unTag, (<|$|>)
  , DefMessage(..), Definition(..), ArrayDefinition(..), StructDefinition(..)
  , TupleDefinition(..), unAssocList, TreeType)
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
    parseJSON v = parseJSON v >>= Path.fromText

instance ToJSON Path where
    toJSON = toJSON . Path.toText

instance FromJSON Seg where
    parseJSON v = parseJSON v >>= Path.mkSeg

instance ToJSON Seg where
    toJSON = toJSON . Path.unSeg

instance FromJSON TypeName where
    parseJSON = withObject "TypeName" $ \o -> TypeName <$> o .: "ns" <*> o .: "seg"

instance ToJSON TypeName where
    toJSON (TypeName ns seg) = object ["ns" .= ns, "seg" .= seg]

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

instance FromJSON ContainerUpdateMessage where
    parseJSON = parseTaggedJson cumtTaggedData $ \e -> case e of
        CUMTPresentAfter -> withObject "MsgPresentAfter" $ \o -> MsgPresentAfter
            <$> o .: "path" <*> o .: "tgt" <*> o .: "ref" <*> o .: "att"
        CUMTAbsent -> withObject "MsgAbsent" $ \o -> MsgAbsent
            <$> o .: "path" <*> o .: "tgt" <*> o .: "att"

instance ToJSON ContainerUpdateMessage where
    toJSON = buildTaggedJson cumtTaggedData $ \v -> case v of
        MsgPresentAfter p tgt ref att -> object ["path" .= p, "tgt" .= tgt, "ref" .= ref, "att" .= att]
        MsgAbsent p tgt att -> object ["path" .= p, "tgt" .= tgt, "att" .= att]

instance FromJSON ToRelayClientBundle where
    parseJSON = withObject "ToRelayClientBundle" $ \b -> ToRelayClientBundle <$> b .: "subs" <*> b .: "data" <*> b .: "cont"

instance (FromJSON a) => FromJSON (TimeStamped a) where
    parseJSON o = withObject "TimeStamped" (\ts -> curry TimeStamped <$> ts .: "time" <*> parseJSON o) o

toRelayClientBundleToClapi :: B.ByteString -> Either String (TimeStamped ToRelayClientBundle)
toRelayClientBundleToClapi s = eitherDecode s

instance ToJSON a => ToJSON (ErrorIndex a) where
    toJSON = buildTaggedJson errIdxTaggedData $ \v -> case v of
        GlobalError -> toJSON (Nothing :: Maybe Int)
        PathError p -> toJSON p
        TimePointError p tpid -> object ["path" .= p, "tpid" .= tpid]
        TypeError i -> toJSON i

instance ToJSON Liberty where
    toJSON l = toJSON $ case l of
        Cannot -> ("cannot" :: String)
        May -> "may"
        Must -> "must"

instance ToJSON a => ToJSON (MsgError a) where
    toJSON (MsgError ei m) = object ["eIdx" .= ei, "msg" .= m]

instance ToJSON TypeMessage where
    toJSON (MsgAssignType p tn l) = object ["path" .= p, "typeName" .= tn, "lib" .= l]

instance ToJSON StructDefinition where
    toJSON (StructDefinition doc al) = object ["doc" .= doc, "stls" .= (asStl <$> unAssocList al)]
      where
        asStl (s, (tn, l)) = object ["seg" .= s, "tn" .= tn, "lib" .= l]

instance ToJSON InterpolationLimit where
    toJSON = buildTaggedJson ilTaggedData $ const $ toJSON (Nothing :: Maybe Text)

instance ToJSON TreeType where
    toJSON = toJSON . ttToText

instance ToJSON TupleDefinition where
    toJSON (TupleDefinition doc types interpLim) = object ["doc" .= doc, "types" .= typeOs, "il" .= interpLim]
      where
        typeOs = asTypeO <$> unAssocList types
        asTypeO (s, tt) = object ["seg" .= s, "ty" .= tt]

instance ToJSON ArrayDefinition where
    toJSON (ArrayDefinition doc cTn cLib) = object ["doc" .= doc, "ctn" .= cTn, "clib" .= cLib]

instance ToJSON Definition where
    toJSON = buildTaggedJson defTaggedData $ \v -> case v of
        TupleDef d -> toJSON d
        StructDef d -> toJSON d
        ArrayDef d -> toJSON d

instance ToJSON a => ToJSON (DefMessage a) where
    toJSON = buildTaggedJson defMsgTaggedData $ \v -> case v of
        MsgDefine a def -> object ["id" .= toJSON a, "def" .= toJSON def]
        MsgUndefine a -> toJSON a

instance ToJSON FromRelayClientBundle where
    toJSON (FromRelayClientBundle tUnsubs dUnsubs errs defs tas dd co) = object
        [ "tu" .= toJSONList tUnsubs
        , "du" .= toJSONList dUnsubs
        , "errs" .= toJSONList errs
        , "defs" .= toJSONList defs
        , "tas" .= toJSONList tas
        , "dd" .= toJSONList dd
        , "co" .= toJSONList co
        ]

fromRelayClientBundleToJson :: FromRelayClientBundle -> B.ByteString
fromRelayClientBundleToJson = encode
