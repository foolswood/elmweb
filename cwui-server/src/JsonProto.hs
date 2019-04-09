module JsonProto (jsonProto) where

import Data.Aeson (encode, eitherDecode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Text.Encoding (decodeUtf8)
import Data.Text (unpack)

import Control.Monad (forever)
import Clapi.Types (SomeFrDigest, SomeTrDigest, TimeStamped)
import Clapi.Protocol (Protocol, waitThen, sendFwd, sendRev)
import JsonConv ()

import Debug.Trace

tt :: String -> B.ByteString -> B.ByteString
tt tag a = trace (tag ++ ": " ++ unpack (decodeUtf8 a)) a

jsonProto ::
    Monad m => Protocol
        SomeFrDigest
        B.ByteString
        (TimeStamped SomeTrDigest)
        B.ByteString
        m ()
jsonProto = forever $ waitThen jsonMash jsonUnmash
  where
    jsonMash b = sendFwd $ tt "->" $ LB.toStrict $ encode b
    jsonUnmash bs = case eitherDecode $ LB.fromStrict $ tt "<-" bs of
        Left s -> error $ "Decode failure: " ++ show bs ++ " - " ++ s
        Right b -> sendRev b
