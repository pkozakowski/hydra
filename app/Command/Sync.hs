{-# LANGUAGE StandaloneDeriving #-}

module Command.Sync where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Map.Static
import Data.Maybe
import Data.Time
import Data.Time.Clock.POSIX
import Market
import Market.Feed
import Market.Ops
import Market.Time
import Numeric.Precision
import Options.Applicative
import System.IO.Lazy qualified as LazyIO
import System.ProgressBar

data SyncOptions = SyncOptions
  { assets :: [String]
  }

syncOptions :: Parser SyncOptions
syncOptions =
  SyncOptions
    <$> some (argument str $ metavar "ASSETS...")

hostName :: String
hostName = "127.0.0.1"

-- syncAsset :: UTCTime -> String -> IO (TimeSeries Price)
-- syncAsset from asset =
--  semToIO $
--    runPrecisionExact $
--      runTimeIO $
--        runPriceFeedForOneToken runPriceFeed asset $
--          since from
--  where
--    runPriceFeed =
--      runFeedWithMongoCache
--        @PriceFeed
--        @HighestBatchablePeriod
--        hostName
--        $ runPriceFeedBinance

listToTQueue :: TQueue (Maybe a) -> [a] -> IO ()
listToTQueue queue xs = do
  forM_ xs $
    atomically
      . writeTQueue queue
      . Just
  atomically $
    writeTQueue queue Nothing

listFromTQueue :: TQueue (Maybe a) -> IO [a]
listFromTQueue queue = do
  maybeElements <-
    LazyIO.run $
      forM (repeat ()) $
        const $
          LazyIO.interleave $
            atomically $
              readTQueue queue
  return $ fromJust <$> takeWhile isJust maybeElements

deriving instance Show a => Show (Progress a)

-- -- | Synchronizes price data, parallelizing over assets.
-- sync :: SyncOptions -> IO ()
-- sync options = do
--  -- Determine the time we're synced up to.
--  coverages <-
--    semToIO $
--      forM (assets options) $
--        cacheCoverage @PriceFeed hostName
--  let from = minimum $ maybe (posixSecondsToUTCTime 0) snd <$> coverages
--  -- Set up the progress bar.
--  now <- getCurrentTime
--  let diff time = floor $ time `diffUTCTime` from
--      progress time = Progress (diff time) (diff now) ()
--      style = defStyle {styleWidth = ConstantWidth 40}
--  progressBar <- newProgressBar style 10 $ progress from
--  -- Create the queues.
--  queues <-
--    forM (assets options) $
--      const newTQueueIO
--  -- Launch the threads.
--  forM_ (zip (assets options) queues) \(asset, queue) ->
--    forkIO $
--      listToTQueue queue
--        =<< seriesToList
--          <$> syncAsset from asset
--  -- Read the data from queues.
--  assetSeries <- mapM listFromTQueue queues
--  -- Interleave the per-asset series to a single series.
--  let eventSeries =
--        sweep $
--          fromList $
--            zip (assets options) assetSeries
--  -- Display sync progress.
--  forM_ eventSeries \(time, _) ->
--    updateProgress progressBar $ const $ progress time
