{-# LANGUAGE TupleSections #-}

module Market.Feed.IBKR where

import Control.Exception (assert)
import Control.Monad
import Data.Function
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Class
import Data.Map.Static
import Data.Time
import Market.Feed
import Market.Feed.Types
import Market.Log
import Market.Types hiding (Value)
import Polysemy
import Polysemy.Embed
import Polysemy.Error
import Foreign.RPC qualified as RPC

data BarField = BidAvg | BidMin | AskAvg | AskMax
  deriving (Eq, Ord)

data Contract = Cash {symbol :: String, currency :: String}
  deriving (Eq, Ord)

data FeedKey = FeedKey {contract :: Contract, barField :: BarField}

runFeedIBKR
  :: forall f r a
   . (Key f ~ FeedKey, Value f ~ FixedScalar, Members [Error String, Log, Embed IO] r)
  => Sem (Feed f : r) a
  -> Sem r a
runFeedIBKR = interpret \case
  Between_ keys period from to ->
    RPC.session "poetry"
      (\port -> ["-C", "python/ibkr", "run", "python/ibkr/ibkr/run.py", show port])
      \sess -> do
        when (period /= Minute) $ throw "runFeedIBKR only works on minute periods"
        let groups = NonEmpty.groupBy ((==) `on` contract) keys
            contracts = contract . NonEmpty.head <$> groups
        contractToMaybeSeries
          :: StaticMap Contract (Maybe (TimeSeries (StaticMap BarField FixedScalar))) <-
          fromList <$> forM contracts \ctr ->
            (ctr,) <$> betweenForContract sess ctr from to
        let keyToMaybeSeries =
              fromList $
                ( \key ->
                    ( key
                    , fmap (! barField key)
                        <$> (contractToMaybeSeries ! contract key)
                    )
                )
                  <$> NonEmpty.toList keys
        pure $ fmap (remap id) <$> mergeFeeds keyToMaybeSeries
  Between1_ key period from to -> do
    between1_UsingBetween_ @f runFeedIBKR key period from to

betweenForContract
  :: forall r
   . (Members [Error String, Log, Embed IO] r)
  => PythonSession
  -> Contract
  -> UTCTime
  -> UTCTime
  -> Sem r (Maybe (TimeSeries (StaticMap BarField FixedScalar)))
betweenForContract = undefined
