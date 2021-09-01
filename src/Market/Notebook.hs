module Market.Notebook
    ( module Evaluation
    , evaluate
    , evaluateOnWindows
    , runPriceFeed
    ) where

import Data.Composition
import Data.Fixed
import Data.List.NonEmpty as NonEmpty
import Data.Record.Hom
import Data.Text.Lazy
import Data.Time.Clock
import Market
import Market.Evaluation hiding (evaluate, evaluateOnWindows)
import qualified Market.Evaluation as Evaluation
import Market.Feed
import Market.Feed.MongoDB
import Market.Feed.PancakeSwap
import Market.Internal.IO
import Market.Time
import Market.Types
import qualified Market.Feed.Price as PriceFeed
import Numeric.Precision
import Polysemy
import Text.Pretty.Simple

runPriceFeed
    :: forall assets res
     . (Labels assets, HasResolution res)
    => res
    -> UTCTime
    -> UTCTime
    -> IO (TimeSeries (Prices assets))
runPriceFeed res from to
    = semToIO
    $ runPrecision res
    $ runTimeIO
    $ PriceFeed.runPriceFeed @assets runPriceVolumeFeed
    $ between from to where
        runPriceVolumeFeed
            = runPriceVolumeFeedWithMongoCache "127.0.0.1"
            $ runPriceVolumeFeedPancakeSwap

evaluate
    :: forall assets c s res r
     .  ( Labels assets
        , Instrument assets c s
        , HasResolution res
        )
    => res
    -> [Metric]
    -> TimeSeries (Prices assets)
    -> Portfolio assets
    -> c
    -> IO Evaluation
evaluate res = semToIO . runPrecision res .:: Evaluation.evaluate

evaluateOnWindows
    :: forall assets c s res r
     .  ( Labels assets
        , Instrument assets c s
        , HasResolution res
        )
    => res
    -> [Metric]
    -> NominalDiffTime
    -> NominalDiffTime
    -> TimeSeries (Prices assets)
    -> Portfolio assets
    -> c
    -> IO EvaluationOnWindows
evaluateOnWindows res
    = semToIO . runPrecision res .::: Evaluation.evaluateOnWindows
