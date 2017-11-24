{-# LANGUAGE OverloadedStrings #-}
module JsonConv (requestBundleToClapi, updateBundleToJson) where
import qualified Data.Set as Set
import Data.Aeson (FromJSON(..), Value, withArray, Value(..), Array(..))
import Data.Aeson.Types (Parser)
import qualified Data.Vector as Vec
import qualified Data.Text as T

import Clapi.Path (Path)
import Path.Parsing (fromText)
import Clapi.TaggedData
import Clapi.Serialisation (subMsgTaggedData, SubMsgType(..))
import Clapi.Types (RequestBundle, UpdateBundle, SubMessage(..))

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
        SubMsgTSub -> UMsgSubscribe <$> parseJSON
        SubMsgTUnsub -> UMsgUnsubscribe <$> parseJSON

-- instance FromJson RequestBundle
--     parseJSON = withObject $ \b -> RequestBundle <$> b .: "subs" <*> b .: "dums"

-- requestBundleToClapi :: T.Text -> RequestBundle
-- requestBundleToClapi = parseJSON

updateBundleToJson :: UpdateBundle -> T.Text
updateBundleToJson = undefined
