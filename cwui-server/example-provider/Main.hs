{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

import Prelude hiding (fail)

import Data.Bifunctor (first)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Int
import Data.Tagged (Tagged(..))
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Chan.Unagi as Q
import Control.Monad (forever, void, foldM)
import Control.Monad.Fail (MonadFail(..))
import Control.Monad.Trans (lift)
import System.Clock (Clock(Monotonic), getTime, toNanoSecs)
import Network.Simple.TCP (connect, Socket, SockAddr)
import Network.Socket.ByteString (send, recv)

import Clapi.TH (segq, pathq)
import Clapi.Types
  ( Path, Seg, Namespace(..), Placeholder(..), isChildOf
  , Time(..), InterpolationLimit(..), Interpolation(..), Editable(..), TimeStamped(..)
  , FrDigest(..), TrDigest(..), TrpDigest(..), trpdEmpty, FrpDigest(..)
  , Definition, structDef, arrayDef, tupleDef
  , unbounded, TreeType(..)
  , alFromList, alToMap
  , TimeSeriesDataOp(..), DataChange(..), DefOp(..)
  , WireValue(..), (<|$|>)
  , produceToRelayBundle, digestFromRelayBundle
  )
import Clapi.Protocol (Protocol, waitThen, sendFwd, sendRev, (<<->), mapProtocol, runProtocolIO)
import Clapi.SerialisationProtocol (serialiser)
import Clapi.Serialisation.Messages ()

ns :: Seg
ns = [segq|example|]

initialDefs :: Map (Tagged Definition Seg) Definition
initialDefs = Map.fromList $ fmap (first Tagged) $
  [ ( [segq|delay|]
    , tupleDef "How long to delay responses" (alFromList [([segq|t|], TtTime)]) ILUninterpolated)
  , ( [segq|tsi|]
    , tupleDef "Timeseries of ints" (alFromList [([segq|i|], TtInt32 unbounded)]) ILLinear)
  , ( [segq|arr|]
    , arrayDef "Editable array of times" Nothing (Tagged [segq|delay|]) Editable)
  , ( ns
    , structDef "Example API for client testing" $ alFromList
      [ ([segq|delay|], (Tagged [segq|delay|], Editable))
      , ([segq|tsi|], (Tagged [segq|tsi|], Editable))
      , ([segq|arr|], (Tagged [segq|arr|], Editable))
      ]
    )
  ]

initDigest :: DummyApiState -> TrpDigest
initDigest das = (trpdEmpty $ Namespace ns)
  { trpdDefinitions = OpDefine <$> initialDefs
  , trpdData = alFromList
    [ ([pathq|/delay|], ConstChange Nothing [WireValue $ dasDelay das])
    , ( [pathq|/tsi|]
      , TimeChange $ Map.singleton 24
        (Nothing, OpSet (Time 1 0) [WireValue (64 :: Int32)] ILinear))
    ]
  }

data Delayed a = Delayed Time a

data DummyApiState = DummyApiState
  { dasDelay :: Time
  }

getHandler :: MonadFail m => Path -> DataChange -> DummyApiState -> m DummyApiState
getHandler p
  | p == [pathq|/delay|] = \dc -> case dc of
    ConstChange _ [dwv] -> \das -> (\d -> das {dasDelay = d}) <|$|> dwv
    _ -> const $ fail "Unexpected number of wvs"
  | isChildOf p [pathq|/arr|] = const pure
  | otherwise = const $ const $ fail "No handler"

apiProto :: MonadFail m => DummyApiState -> Protocol FrpDigest (Delayed TrpDigest) TrpDigest TrpDigest m ()
apiProto das = sendRev (initDigest das) >> steadyState das
  where
    steadyState das = waitThen fwd rev
    handlerFor acc (p, v) = getHandler p v acc
    fwd (FrpDigest tgtNs dd _creates cops) =
      let
        cops' = (fmap . fmap . fmap . fmap) (either unPlaceholder id) cops
      in do
        das' <- foldM (\acc pv -> lift $ handlerFor acc pv) das $ Map.toList $ alToMap dd
        sendFwd $ Delayed (dasDelay das') $
          (trpdEmpty tgtNs) {trpdData = dd, trpdContOps = cops'}
        steadyState das'
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
        Frcrd _ -> fwdProviderProto
        _ -> return ()
    rev d = do
        sendRev $ Trpd d
        fwdProviderProto

-- Convert a time to a number of microseconds
usTime :: Time -> Int
usTime (Time s f) = fromIntegral $ (s * 1000000) + (round $ toRational (f * 1000000) / 2^32)

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
