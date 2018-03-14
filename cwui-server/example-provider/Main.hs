{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Int
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Chan.Unagi as Q
import Control.Monad (forever, void)
import Control.Monad.Trans (lift)
import System.Clock (TimeSpec(..), Clock(Monotonic), getTime, toNanoSecs)
import Network.Simple.TCP (connect, Socket, SockAddr)
import Network.Socket.ByteString (send, recv)

import Clapi.TH (segq, pathq)
import Clapi.Types
  ( Seg, TypeName(..), Time(..), InterpolationLimit(..), Interpolation(..), Liberty(..), TimeStamped(..)
  , FrDigest(..), TrDigest(..), TrpDigest(..), FrpDigest(..)
  , Definition, structDef, arrayDef, tupleDef
  , unbounded, ttInt32, ttTime
  , alFromList
  , TimeSeriesDataOp(..), DataChange(..), DefOp(..)
  , WireValue(..)
  , produceToRelayBundle, digestFromRelayBundle
  )
import Clapi.Protocol (Protocol, waitThen, sendFwd, sendRev, (<<->), mapProtocol, runProtocolIO)
import Clapi.SerialisationProtocol (serialiser)
import Clapi.Serialisation.Messages ()

ns :: Seg
ns = [segq|example|]

initialDefs :: Map Seg Definition
initialDefs = Map.fromList
  [ ([segq|delay|], tupleDef "How long to delay responses" (alFromList [([segq|t|], ttTime)]) ILUninterpolated)
  , ([segq|tsi|], tupleDef "Timeseries of ints" (alFromList [([segq|i|], ttInt32 unbounded)]) ILLinear)
  , (ns, structDef "Example API for client testing" $ alFromList
      [ ([segq|delay|], (TypeName ns [segq|delay|], May))
      , ([segq|tsi|], (TypeName ns [segq|tsi|], May))
      ]
    )
  ]

initDigest :: DummyApiState -> TrpDigest
initDigest das = TrpDigest ns (OpDefine <$> initialDefs) initDd initCops mempty
  where
    initDd = alFromList
      [ ([pathq|/delay|], ConstChange Nothing [WireValue $ dasDelay das])
      , ([pathq|/tsi|], TimeChange $ Map.singleton 24 (Nothing, OpSet (Time 1 0) [WireValue (64 :: Int32)] ILinear))
      ]
    initCops = mempty

data Delayed a = Delayed Time a

data DummyApiState = DummyApiState
  { dasDelay :: Time
  }

apiProto :: Monad m => DummyApiState -> Protocol FrpDigest (Delayed TrpDigest) TrpDigest TrpDigest m ()
apiProto das = sendRev (initDigest das) >> steadyState das
  where
    steadyState das = waitThen fwd rev
    fwd (FrpDigest tgtNs dd cops) = do
        -- TODO: Extract info, update state
        sendFwd $ Delayed (dasDelay das) $ TrpDigest tgtNs mempty dd cops mempty
        steadyState das
    rev trpd = do
        sendRev trpd
        steadyState das

fwdProviderProto :: Monad m => Protocol FrDigest FrpDigest TrDigest TrpDigest m ()
fwdProviderProto = waitThen fwd rev
  where
    fwd d = case d of
        Frpd pd -> do
            sendFwd pd
            fwdProviderProto
        _ -> return ()
    rev d = do
        sendRev $ Trpd d
        fwdProviderProto

-- Convert a time to a number of microseconds
usTime :: Time -> Int
usTime (Time s f) = fromIntegral $ (s * 1000000) + (fromIntegral $ f * (1000000 `div` 2^32))

delayer :: IO (Delayed a -> IO (), IO a)
delayer = do
    (ic, oc) <- Q.newChan
    return (Q.writeChan ic, delayedRead oc)
  where
    delayedRead c = do
        Delayed t v <- Q.readChan c
        threadDelay $ usTime t
        return v

-- FIXME: Duplication between this and the one in the FFI provider
timestampOutbound :: Protocol a a (TimeStamped b) b IO ()
timestampOutbound = forever $ waitThen sendFwd rev
  where
    rev b = do
        t <- lift $ getTime Monotonic
        sendRev $ TimeStamped (toTime t, b)
    toTime ts =
      let
        ns = realToFrac $ toNanoSecs ts :: Double
        (s, f) = properFraction $ ns / 10^(9 :: Int)
        ss = round $ 2^(32 :: Int) * f
      in Time s ss

onSocketConnected :: (Socket, SockAddr) -> IO ()
onSocketConnected (sock, _) = do
    (delayIn, delayOut) <- delayer
    runProtocolIO (recv sock 1024) delayIn (void . send sock) delayOut proto
  where
    startState = DummyApiState {dasDelay = Time 1 0}
    proto =
        serialiser <<-> timestampOutbound <<->
        mapProtocol digestFromRelayBundle produceToRelayBundle <<->
        fwdProviderProto <<-> apiProto startState

main :: IO ()
main = connect "127.0.0.1" "1234" onSocketConnected
