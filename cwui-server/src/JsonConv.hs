{-# LANGUAGE OverloadedStrings #-}
module JsonConv (requestBundleToClapi, updateBundleToJson) where
import qualified Data.Set as Set
import Data.Aeson (FromJSON(..), Value, withArray, Value(..), Array(..), withObject, (.:), decode)
import Data.Aeson.Types (Parser)
import qualified Data.Vector as Vec
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T

import Clapi.Path (Path)
import Path.Parsing (fromText)
import Clapi.TaggedData
import Clapi.Serialisation (subMsgTaggedData, SubMsgType(..), cvTaggedData, interpolationTaggedData, dumtTaggedData, DataUpdateMsgType(..))
import Clapi.Types (RequestBundle, UpdateBundle, SubMessage(..), Time(..), ClapiValue(..), ClapiTypeEnum(..), InterpolationType(..), Interpolation(..), DataUpdateMessage(..), RequestBundle(..))

parseTaggedJson :: TaggedData e a -> (e -> Value -> Parser a) -> Value -> Parser a
parseTaggedJson td p = withArray "Tagged" (handleTagged . arrayAsList)
  where
    tagSet = Set.fromList $ tdAllTags td
    arrayAsList v = Vec.toList v
    handleTagged [jt, v] = do
        ts <- T.unpack <$> parseJSON jt
        t <- case ts of
            [c] -> return c
            _ -> fail "Tag not single char"
        if t `elem` tagSet
            then p (tdTagToEnum td t) v
            else fail $ "Bad tag"
    handleTagged _ = fail "Invalid tagged value"

instance FromJSON Path where
    parseJSON v = parseJSON v >>= fromText

instance FromJSON SubMessage where
    parseJSON = parseTaggedJson subMsgTaggedData $ \e -> case e of
        SubMsgTSub -> fmap UMsgSubscribe . parseJSON
        SubMsgTUnsub -> fmap UMsgUnsubscribe . parseJSON

instance FromJSON Time where
    parseJSON = do
        s <- parseJSON
        f <- parseJSON
        return $ Time <$> s <*> f

instance FromJSON ClapiValue where
    parseJSON = parseTaggedJson cvTaggedData $ \e -> case e of
        ClTTime -> fmap ClTime . parseJSON
        ClTEnum -> fmap ClEnum . parseJSON
        ClTWord32 -> fmap ClWord32 . parseJSON
        ClTWord64 -> fmap ClWord64 . parseJSON
        ClTInt32 -> fmap ClInt32 . parseJSON
        ClTInt64 -> fmap ClInt64 . parseJSON
        ClTFloat -> fmap ClFloat . parseJSON
        ClTDouble -> fmap ClDouble . parseJSON
        ClTString -> fmap ClString . parseJSON
        ClTList -> fmap ClList . parseJSONList

instance FromJSON Interpolation where
    parseJSON = parseTaggedJson interpolationTaggedData $ \e v -> case e of
        ITConstant -> return IConstant
        ITLinear -> return ILinear

instance FromJSON DataUpdateMessage where
    parseJSON = parseTaggedJson dumtTaggedData $ \e -> case e of
        DUMTAdd -> do
            p <- parseJSON
            t <- parseJSON
            vs <- parseJSONList
            i <- parseJSON
            return $ UMsgAdd <$> p <*> t <*> vs <*> i <*> return Nothing <*> return Nothing
        DUMTSet -> do
            p <- parseJSON
            t <- parseJSON
            vs <- parseJSONList
            i <- parseJSON
            return $ UMsgSet <$> p <*> t <*> vs <*> i <*> return Nothing <*> return Nothing

instance FromJSON RequestBundle where
    parseJSON = withObject "RequestBundle" $ \b -> RequestBundle <$> b .: "subs" <*> b .: "dums"

requestBundleToClapi :: B.ByteString -> Maybe RequestBundle
requestBundleToClapi = decode

updateBundleToJson :: UpdateBundle -> B.ByteString
updateBundleToJson = undefined
