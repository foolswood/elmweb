{-# LANGUAGE
    QuasiQuotes
  , OverloadedStrings
  , GADTs
  , DataKinds
#-}

import Prelude hiding (fail)

import Data.Bifunctor (first)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Tagged (Tagged(..))
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Chan.Unagi as Q
import Control.Monad (forever, void, foldM)
import Control.Monad.Fail (MonadFail(..))
import Control.Monad.Trans (lift)
import System.Clock (Clock(Monotonic), getTime, toNanoSecs)
import Network.Simple.TCP (connect, Socket, SockAddr)
import Network.Socket.ByteString (send, recv)

import Clapi.TH (n, pathq)
import Clapi.Types
  ( Path, Namespace, Placeholder(..), isChildOf
  , Time(..), InterpolationType(..), Interpolation(..), Editability(..), TimeStamped(..)
  , FrDigest(..), TrDigest(..), TrpDigest, trpdEmpty, FrpDigest
  , SomeDefinition, DefName, structDef, arrayDef, tupleDef
  , unbounded, ttTime, ttInt32
  , TimeSeriesDataOp(..), DataChange(..), DefOp(..)
  , someWv, WireType(..), castWireValue
  , SomeFrDigest(..), SomeTrDigest(..)
  , castName
  , OrderedContOps, ContOps
  )
import qualified Clapi.Types.AssocList as AL
import Clapi.Protocol (Protocol, waitThen, sendFwd, sendRev, (<<->), runProtocolIO)
import Clapi.SerialisationProtocol (serialiser)
import Clapi.Serialisation.Digests ()

ns :: Namespace
ns = [n|example|]

initialDefs :: Map DefName SomeDefinition
initialDefs = Map.fromList
  [ ( [n|delay|]
    , tupleDef "How long to delay responses" (AL.fromList [([n|t|], ttTime)]) Nothing)
  , ( [n|tsi|]
    , tupleDef "Timeseries of ints" (AL.fromList [([n|i|], ttInt32 unbounded)]) $ Just ItLinear)
  , ( [n|arr|]
    , arrayDef "Editable array of times" Nothing [n|delay|] Editable)
  , ( castName ns
    , structDef "Example API for client testing" $ AL.fromList
      [ ([n|delay|], ([n|delay|], Editable))
      , ([n|tsi|], ([n|tsi|], Editable))
      , ([n|arr|], ([n|arr|], Editable))
      ]
    )
  ]

initDigest :: DummyApiState -> TrpDigest
initDigest das = (trpdEmpty ns)
  { trpdDefs = OpDefine <$> initialDefs
  , trpdData = AL.fromList
    [ ([pathq|/delay|], ConstChange Nothing [someWv WtTime $ dasDelay das])
    , ( [pathq|/tsi|]
      , TimeChange $ Map.singleton 24
        (Nothing, OpSet (Time 1 0) [someWv WtInt32 64] ILinear))
    ]
  }

data Delayed a = Delayed Time a

data DummyApiState = DummyApiState
  { dasDelay :: Time
  }

getHandler :: MonadFail m => Path -> DataChange -> DummyApiState -> m DummyApiState
getHandler p
  | p == [pathq|/delay|] = \dc -> case dc of
    ConstChange _ [dwv] -> \das -> (\d -> das {dasDelay = d}) <$> castWireValue dwv
    _ -> const $ fail "Unexpected number of wvs"
  | isChildOf p [pathq|/arr|] = const pure
  | otherwise = const $ const $ fail "No handler"

apiProto :: MonadFail m => DummyApiState -> Protocol FrpDigest (Delayed TrpDigest) TrpDigest TrpDigest m ()
apiProto das = sendRev (initDigest das) >> steadyState das
  where
    steadyState das = waitThen fwd rev
    handlerFor acc (p, v) = getHandler p v acc
    fwd (Frpd tgtNs dd _creates orderedCops) =
      let
        cops = mapContOpKey (either castName id) $ unOrder orderedCops
        unOrder :: Ord i => OrderedContOps i -> ContOps i
        unOrder = fmap AL.toMap
        mapContOpKey :: Ord j => (i -> j) -> ContOps i -> ContOps j
        mapContOpKey f = fmap (Map.mapKeys f . (fmap . fmap . fmap) f)
      in do
        das' <- foldM (\acc pv -> lift $ handlerFor acc pv) das $ Map.toList $ AL.toMap dd
        sendFwd $ Delayed (dasDelay das') $
          (trpdEmpty tgtNs) {trpdData = dd, trpdContOps = cops}
        steadyState das'
    rev trpd = do
        sendRev trpd
        steadyState das

fwdProviderProto :: Monad m => Protocol SomeFrDigest FrpDigest SomeTrDigest TrpDigest m ()
fwdProviderProto = waitThen fwd rev
  where
    fwd :: Monad m => SomeFrDigest -> Protocol SomeFrDigest FrpDigest SomeTrDigest TrpDigest m ()
    fwd (SomeFrDigest d) = case d of
        Frpd {} -> do
            sendFwd d
            fwdProviderProto
        Frcrd {} -> fwdProviderProto
        _ -> return ()
    rev d = do
        sendRev $ SomeTrDigest d
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
        fwdProviderProto <<-> apiProto startState

main :: IO ()
main = connect "127.0.0.1" "1234" onSocketConnected
