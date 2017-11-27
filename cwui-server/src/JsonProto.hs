module JsonProto (jsonProto) where
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

import Clapi.Types (FromRelayBundle(..), ToRelayBundle(..))
import Clapi.Protocol (Protocol, waitThen, sendFwd, sendRev)
import JsonConv

jsonProto ::
    Monad m => Protocol
        FromRelayBundle
        B.ByteString
        ToRelayBundle
        B.ByteString
        m ()
jsonProto = waitThen jsonMash jsonUnmash
  where
    jsonMash (FRBClient b) = sendFwd $ LB.toStrict $ updateBundleToJson b
    jsonUnmash bs = case requestBundleToClapi $ LB.fromStrict bs of
        Nothing -> error "Decode failure"
        (Just b) -> sendRev $ TRBClient b
