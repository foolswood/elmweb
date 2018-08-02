module JsonProto (jsonProto) where

import Data.Aeson (encode, eitherDecode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

import Control.Monad (forever)
import Clapi.Types (FromRelayBundle(..), ToRelayBundle(..), TimeStamped(..))
import Clapi.Protocol (Protocol, waitThen, sendFwd, sendRev)
import JsonConv

jsonProto ::
    Monad m => Protocol
        FromRelayBundle
        B.ByteString
        (TimeStamped ToRelayBundle)
        B.ByteString
        m ()
jsonProto = forever $ waitThen jsonMash jsonUnmash
  where
    jsonMash (Frcrb b) = sendFwd $ LB.toStrict $ encode b
    jsonMash (Frcsb b) = sendFwd $ LB.toStrict $ encode b
    jsonMash (Frcub b) = sendFwd $ LB.toStrict $ encode b
    jsonMash _ = return ()
    jsonUnmash bs = case eitherDecode $ LB.fromStrict bs of
        Left s -> error $ "Decode failure: " ++ (show bs) ++ " - " ++ s
        Right (TimeStamped (t, b)) -> sendRev $ TimeStamped (t, b)
