module JsonProto (jsonProto) where

import Data.Aeson (encode, eitherDecode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

import Control.Monad (forever)
import Clapi.Types (FromRelayBundle(..), ToRelayBundle(..), TimeStamped(..))
import Clapi.Protocol (Protocol, waitThen, sendFwd, sendRev)
import JsonConv ()

import Debug.Trace

tt :: Show a => String -> a -> a
tt tag a = trace (tag ++ ": " ++ show a) a

jsonProto ::
    Monad m => Protocol
        FromRelayBundle
        B.ByteString
        (TimeStamped ToRelayBundle)
        B.ByteString
        m ()
jsonProto = forever $ waitThen jsonMash jsonUnmash
  where
    jsonMash b = sendFwd $ tt "->" $ LB.toStrict $ encode b
    jsonUnmash bs = case eitherDecode $ LB.fromStrict $ tt "<-" bs of
        Left s -> error $ "Decode failure: " ++ (show bs) ++ " - " ++ s
        Right b -> sendRev b
