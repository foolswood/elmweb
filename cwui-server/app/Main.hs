module Main where
import Data.ByteString (ByteString)
import Data.Maybe (fromJust)
import Control.Monad (void)
import qualified Network.Simple.TCP as TCP
import qualified Network.WebSockets as WS

import Clapi.Protocol (Protocol, (<<->), runProtocolIO)
import Clapi.SerialisationProtocol (serialiser)
import JsonProto (jsonProto)

formatConvProto ::
    Monad m => Protocol
        ByteString
        ByteString
        ByteString
        ByteString
        m ()
formatConvProto = serialiser <<-> jsonProto

bridge :: WS.Connection -> (TCP.Socket, TCP.SockAddr) -> IO ()
bridge ws (s, _) = runProtocolIO
    (fromJust <$> TCP.recv s 1024)
    (WS.sendTextData ws)
    (void . TCP.send s)
    (WS.receiveData ws)
    formatConvProto

connectToRelay :: WS.Connection -> IO ()
connectToRelay ws = TCP.connect "127.0.0.1" "1234" (bridge ws)

acceptWs :: WS.PendingConnection -> IO ()
acceptWs p = WS.acceptRequest p >>= connectToRelay

main :: IO ()
main = WS.runServer "127.0.0.1" 8004 acceptWs
