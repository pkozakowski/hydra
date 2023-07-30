module Market.Feed.IBKR where

import Data.Map.Class
import Data.Time
import Market.Feed
import Market.Log
import Market.Types
import Polysemy
import Polysemy.Embed
import Polysemy.Error

data BarField = BidAvg | BidMin | AskAvg | AskMax
data Contract = Cash {symbol :: String, currency :: String}
data FeedKey = FeedKey {contract :: Contract, barField :: BarField}

runFeedIBKR
  :: forall f r a
   . (Key f ~ FeedKey, Members [Error String, Log, Embed IO] r)
  => Sem (Feed f : r) a
  -> Sem r a
runFeedIBKR = interpret \case
  Between_ keys period from to -> do
    -- between_UsingBetween1_ runFeedIBKR keys period from to

    pure undefined
  Between1_ key period from to -> do
    pure undefined

betweenForContract
  :: forall f r
   . (Key f ~ BarField, Members [Error String, Log, Embed IO] r)
  => Contract
  -> UTCTime
  -> UTCTime
  -> Sem r (Maybe (TimeSeries f))
betweenForContract = undefined
