
module Market.Notebook
    ( module Evaluation
    , module Tuning
    , evaluate
    , evaluateOnWindows
    , runPriceFeed
    , runPriceFeedEver
    , tune
    ) where

import Control.Logging
import Data.Composition
import Data.Fixed
import Data.List.NonEmpty as NonEmpty
import Data.Text
import qualified Data.Text.Lazy as L
import Data.Time.Clock
import Data.Time.Clock.POSIX
import qualified Dhall.Pretty as Dh
import Market
import Market.Dhall
import Market.Evaluation hiding (evaluate, evaluateOnWindows)
import qualified Market.Evaluation as Evaluation
import Market.Feed
import Market.Feed.Binance
import Market.Feed.MongoDB
import Market.Internal.IO
import Market.Time
import Market.Tuning hiding (tune)
import qualified Market.Tuning as Tuning
import Market.Types
import Market.Feed.Price (PriceFeed)
import qualified Market.Feed.Price as PriceFeed
import Numeric.Precision
import Polysemy
import Polysemy.Error
import Polysemy.Output
import Prelude hiding (log)

runPriceFeed
    :: forall atp res
     . (BatchablePeriod atp, HasResolution res)
    => res
    -> [Asset]
    -> UTCTime
    -> UTCTime
    -> IO (TimeSeries Prices)
runPriceFeed res assets from to
    = semToIO
    $ runPrecision res
    $ runTimeIO
    $ PriceFeed.runPriceFeed assets runPriceFeed
    $ between from to where
        runPriceFeed
            :: String
            -> Sem [Feed Double, Time, Precision, Error String, Embed IO] a
            -> Sem [Time, Precision, Error String, Embed IO] a
        runPriceFeed
            = runFeedWithMongoCache @PriceFeed @atp "127.0.0.1"
            $ runPriceFeedBinance

runPriceFeedEver
    :: forall atp res
     . (BatchablePeriod atp, HasResolution res)
    => res -> [Asset] -> IO (TimeSeries Prices)
runPriceFeedEver res assets = do
    let from = posixSecondsToUTCTime 0
    to <- getCurrentTime
    runPriceFeed @atp res assets from to

evaluate
    :: forall c s res r
     .  ( Instrument c s
        , HasResolution res
        )
    => res
    -> [Metric]
    -> Fees
    -> TimeSeries Prices
    -> Portfolio
    -> c
    -> IO Evaluation
evaluate res
    = semToIOPure @MarketError
    . runPrecision res
 .::. Evaluation.evaluate

evaluateOnWindows
    :: forall c s res r
     .  ( Instrument c s
        , HasResolution res
        )
    => res
    -> [Metric]
    -> Fees
    -> NominalDiffTime
    -> NominalDiffTime
    -> TimeSeries (Prices)
    -> Portfolio
    -> c
    -> IO EvaluationOnWindows
evaluateOnWindows res
        = semToIOPure @(MarketError)
        . runPrecision res
    .:::. Evaluation.evaluateOnWindows

tune
    :: forall c s res e
     . (Instrument c s, Show c, HasResolution res, Show e)
    => res
    -> StopWhen
    -> ( c -> Sem
            [ Output (c, Double)
            , Precision
            , Time
            , Error e
            , Embed IO
            ] Double
       )
    -> (Grid c -> NonEmpty c)
    -> Grid c
    -> IO (c, Double)
tune res
    = semToIO
    . runTimeIO
    . runPrecision res
    . runOutputLog
  .:: Tuning.tune where
        runOutputLog = runOutputSem \(config, ftn) -> embed $ log
            $ "new best config [fitness = " <> pack (show ftn) <> "]:\n"
           <> pack (showPrettyExpr $ smartToDhall config)
