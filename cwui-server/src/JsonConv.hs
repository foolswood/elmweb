{-# LANGUAGE OverloadedStrings #-}
module JsonConv (requestBundleToClapi, updateBundleToJson) where
import qualified Data.Set as Set
import Data.Aeson (
    FromJSON(..), ToJSON(..), Value, withArray, Value(..), Array(..),
    withObject, (.:), eitherDecode, object, (.=), encode)
import Data.Aeson.Types (Parser)
import qualified Data.Vector as Vec
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T

import Clapi.Path (Path)
import Path.Parsing (fromText, toText)
import Clapi.TaggedData
import Clapi.Serialisation (
    subMsgTaggedData, SubMsgType(..), cvTaggedData, interpolationTaggedData,
    dumtTaggedData, DataUpdateMsgType(..), tumtTaggedData)
import Clapi.Types (
    RequestBundle, UpdateBundle, SubMessage(..), Time(..), ClapiValue(..),
    ClapiTypeEnum(..), InterpolationType(..), Interpolation(..),
    DataUpdateMessage(..), RequestBundle(..), TreeUpdateMessage(..),
    OwnerUpdateMessage(..), UMsgError(..), UpdateBundle(..), TimeStamped(..))

parseTaggedJson :: TaggedData e a -> (e -> Value -> Parser a) -> Value -> Parser a
parseTaggedJson td p = withArray "Tagged" (handleTagged . Vec.toList)
  where
    tagSet = Set.fromList $ tdAllTags td
    handleTagged [jt, v] = do
        ts <- T.unpack <$> parseJSON jt
        t <- case ts of
            [c] -> return c
            _ -> fail "Tag not single char"
        if t `elem` tagSet
            then p (tdTagToEnum td t) v
            else fail $ "Bad tag '" ++ [t] ++ "' expecting '" ++ (Set.toList tagSet) ++ "'"
    handleTagged _ = fail "Invalid tagged value"

instance FromJSON Path where
    parseJSON v = parseJSON v >>= fromText

instance ToJSON Path where
    toJSON = toJSON . toText

instance FromJSON SubMessage where
    parseJSON = parseTaggedJson subMsgTaggedData $ \e -> case e of
        SubMsgTSub -> fmap UMsgSubscribe . parseJSON
        SubMsgTUnsub -> fmap UMsgUnsubscribe . parseJSON

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

instance ToJSON ClapiValue where
    toJSON = buildTaggedJson cvTaggedData $ \v -> case v of
        (ClTime t) -> toJSON t
        (ClEnum e) -> toJSON e
        (ClWord32 w) -> toJSON w
        (ClWord64 w) -> toJSON w
        (ClInt32 i) -> toJSON i
        (ClInt64 i) -> toJSON i
        (ClFloat f) -> toJSON f
        (ClDouble f) -> toJSON f
        (ClString s) -> toJSON s
        (ClList l) -> toJSONList l

instance FromJSON Interpolation where
    parseJSON = parseTaggedJson interpolationTaggedData $ \e v -> case e of
        ITConstant -> return IConstant
        ITLinear -> return ILinear

instance ToJSON Interpolation where
    toJSON = buildTaggedJson interpolationTaggedData $ \v -> toJSON (Nothing :: Maybe Int)

instance FromJSON DataUpdateMessage where
    parseJSON = parseTaggedJson dumtTaggedData $ \e -> withObject "Message" $ case e of
        DUMTSet -> \v -> UMsgSet <$> v .: "path" <*> v .: "time" <*> v .: "args" <*> v .: "interpolation" <*> return Nothing <*> return Nothing

instance ToJSON DataUpdateMessage where
    toJSON = buildTaggedJson dumtTaggedData $ \i -> case i of
        (UMsgAdd p t vs i ma ms) -> object ["path" .= p, "time" .= t, "args" .= vs, "interpolation" .= i]
        (UMsgSet p t vs i ma ms) -> object ["path" .= p, "time" .= t, "args" .= vs, "interpolation" .= i]
        (UMsgSetChildren p ns ma) -> object ["path" .= p, "names" .= ns]

instance FromJSON RequestBundle where
    parseJSON = withObject "RequestBundle" $ \b -> RequestBundle <$> b .: "subs" <*> b .: "dums"

instance (FromJSON a) => FromJSON (TimeStamped a) where
    parseJSON o = withObject "TimeStamped" (\ts -> curry TimeStamped <$> ts .: "time" <*> parseJSON o) o

requestBundleToClapi :: B.ByteString -> Either String (TimeStamped RequestBundle)
requestBundleToClapi s = eitherDecode s

instance ToJSON TreeUpdateMessage where
    toJSON = buildTaggedJson tumtTaggedData $ \i -> case i of
        (UMsgAssignType p tp) -> object ["path" .= p, "type" .= tp]

instance ToJSON UMsgError where
    toJSON (UMsgError p m) = object ["path" .= p, "msg" .= m]

jsonifyOums :: [OwnerUpdateMessage] -> Value
jsonifyOums oums = toJSON $ map jsonify oums
  where
    jsonify (Left tum) = toJSON tum
    jsonify (Right dum) = toJSON dum

instance ToJSON UpdateBundle where
    toJSON (UpdateBundle errs oums) = object ["errs" .= toJSONList errs, "ups" .= jsonifyOums oums]

updateBundleToJson :: UpdateBundle -> B.ByteString
updateBundleToJson = encode
