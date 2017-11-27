module Main where
import Data.ByteString (ByteString)

import Clapi.Protocol (Protocol, (<<->))
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

main :: IO ()
main = do
  putStrLn "hello world"
