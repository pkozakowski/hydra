{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Market.Notebook
    ( module Evaluation
    , evaluate
    , evaluateOnWindows
    , plotSeries
    , plotTree
    , runPriceFeed
    , runPriceFeedEver
    ) where

import Data.Composition
import Data.Fixed
import Data.List.NonEmpty as NonEmpty
import Data.Record.Hom
import Data.Text
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Graphics.Vega.VegaLite
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
import Polysemy.Error

data PriceFeed

instance FeedType PriceFeed where
    feedName = "price"

runPriceFeed
    :: forall assets atp res
     . (Labels assets, BatchablePeriod atp, HasResolution res)
    => res
    -> UTCTime
    -> UTCTime
    -> IO (TimeSeries (Prices assets))
runPriceFeed res from to
    = semToIO
    $ runPrecision res
    $ runTimeIO
    $ PriceFeed.runPriceFeed @assets runPriceFeed
    $ between from to where
        runPriceFeed
            :: String
            -> Sem [Feed Double, Time, Precision, Error String, Embed IO] a
            -> Sem [Time, Precision, Error String, Embed IO] a
        runPriceFeed
            = runFeedWithMongoCache @PriceFeed @atp "127.0.0.1"
            $ runPriceFeedPancakeSwap

runPriceFeedEver
    :: forall assets atp res
     . (Labels assets, BatchablePeriod atp, HasResolution res)
    => res -> IO (TimeSeries (Prices assets))
runPriceFeedEver res = do
    let from = posixSecondsToUTCTime 0
    to <- getCurrentTime
    runPriceFeed @assets @atp res from to

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
evaluate res = semToIOPure . runPrecision res .:: Evaluation.evaluate

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
    = semToIOPure . runPrecision res .::: Evaluation.evaluateOnWindows

defaultBackground :: PropertySpec
defaultBackground = background "rgba(0, 0, 0, 0.03)"

plotSeries :: TimeSeries Double -> [PropertySpec]
plotSeries series =
    [ width 600
    , height 300
    , dt []
    , enc []
    , mark Line [MTooltip TTEncoding]
    , defaultBackground
    ] where
        dt = seriesToVega series where
            seriesToVega (TimeSeries txs)
                = dataFromColumns []
                . dataColumn "time"
                    (Numbers $ NonEmpty.toList $ utcToVega . fst <$> txs)
                . dataColumn "value"
                    (Numbers $ NonEmpty.toList $ snd <$> txs)

        enc = encoding
            . position X [ PName "time", PmType Temporal ]
            . position Y [ PName "value", PmType Quantitative ]

plotTree :: InstrumentTree (TimeSeries Double) -> [PropertySpec]
plotTree tree =
    [ width 600
    , height 300
    , dt []
    , enc []
    , mark Line [MTooltip TTEncoding]
    , defaultBackground
    ] where
        dt = flatTreeToVega $ flattenTree tree where
            flatTreeToVega instrSeries = dataFromColumns []
                . dataColumn "time"
                    (Numbers $ mapTimeStep (utcToVega . fst) =<< instrSeries)
                . dataColumn "value"
                    (Numbers $ mapTimeStep snd =<< instrSeries)
                . dataColumn "instrument"
                    (Strings $ constAlongSeries fst =<< instrSeries)
                where
                    mapTimeStep f
                        = NonEmpty.toList . fmap f . unTimeSeries . snd
                    constAlongSeries f = Prelude.replicate
                        <$> NonEmpty.length . unTimeSeries . snd
                        <*> pack . unInstrumentName . f

        enc = encoding
            . position X [PName "time", PmType Temporal]
            . position Y [PName "value", PmType Quantitative]
            . color [MName "instrument"]

utcToVega :: UTCTime -> Double
utcToVega time
    = fromIntegral $ floor $ utcTimeToPOSIXSeconds time Prelude.* 1000
