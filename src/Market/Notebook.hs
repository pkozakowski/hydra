module Market.Notebook
    ( module Evaluation
    , evaluate
    , runPricesFeed
    ) where

import Data.Composition
import Data.Fixed
import Data.List.NonEmpty as NonEmpty
import Data.Record.Hom
import Data.Time.Clock
import Market
import Market.Evaluation hiding (evaluate)
import qualified Market.Evaluation as Evaluation
import Market.Feed
import Market.Feed.MongoDB
import Market.Feed.PancakeSwap
import Market.Internal.IO
import Market.Time
import Market.Types
import qualified Market.Feed.Prices as Prices
import Numeric.Precision
import Polysemy

runPricesFeed
    :: forall assets res
     . (Labels assets, HasResolution res)
    => res
    -> UTCTime
    -> UTCTime
    -> IO (Maybe (TimeSeries (Prices assets)))
runPricesFeed res from to
    = fmap (fmap forceSeries)
    $ semToIO
    $ runPrecision res
    $ runTimeIO
    $ Prices.runPricesFeed @assets runPriceVolumeFeed
    $ between from to where
        runPriceVolumeFeed
            = runPriceVolumeFeedWithMongoCache "127.0.0.1"
            $ runPriceVolumeFeedPancakeSwap
        forceSeries series
            = NonEmpty.length (unTimeSeries series) `seq` series

evaluate
    :: forall assets c s r
     .  ( Labels assets
        , Instrument assets c s
        )
    => TimeSeries (Prices assets)
    -> Portfolio assets
    -> c
    -> [Metric]
    -> IO Evaluation
evaluate = semToIO .:: Evaluation.evaluate
