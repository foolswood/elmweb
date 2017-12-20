module JsonProto (jsonProto) where
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
    jsonMash (FRBClient b) = sendFwd $ LB.toStrict $ updateBundleToJson b
    jsonUnmash bs = case requestBundleToClapi $ LB.fromStrict bs of
        Left s -> error $ "Decode failure: " ++ (show bs) ++ " - " ++ s
        Right (TimeStamped (t, b)) -> sendRev $ TimeStamped (t, TRBClient b)
