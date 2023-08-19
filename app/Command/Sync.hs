{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module Command.Sync where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Static
import Data.Maybe
import Data.Text (Text)
import Data.Time
import Data.Time.Clock.POSIX
import Debug.Trace.Pretty
import Market
import Market.Feed
import Market.Feed.DB (DBPath)
import Market.Feed.Dispatch
import Market.Feed.IBKR
import Market.Feed.Types
import Market.Ops
import Market.Time
import Numeric.Precision
import Options.Applicative
import Polysemy
import Polysemy.Error
import Polysemy.Logging
import System.IO

dbPath :: DBPath
dbPath = "db"

period :: Period
period = Minute

baseAsset :: Asset
baseAsset = "USD"

batchDuration :: NominalDiffTime
batchDuration = 2 * nominalDay

newtype SyncOptions = SyncOptions
  { assets :: [String]
  }

syncOptions :: Parser SyncOptions
syncOptions =
  SyncOptions
    <$> some (argument str $ metavar "ASSETS...")

sync :: forall r. Members [Error String, Logging, Final IO] r => SyncOptions -> Sem r ()
sync SyncOptions {..} = do
  time <- embedFinal getCurrentTime
  fetchBatch
    dbPath
    Minute
    baseAsset
    (negate batchDuration `addUTCTime` time)
    time
    (NonEmpty.fromList $ Asset <$> assets)
  embedFinal $ putStrLn ""

fetchBatch
  :: Members [Error String, Logging, Final IO] r
  => DBPath
  -> Period
  -> Asset
  -> UTCTime
  -> UTCTime
  -> NonEmpty Asset
  -> Sem r ()
fetchBatch dbPath period baseAsset from to assets = forM_ assets \asset -> do
  embedFinal do
    putStr "\r\ESC[K" -- move to start of the line and erase everything to the right
    putStr $ show asset <> "/" <> show baseAsset <> " @ " <> show to <> "  "
    hFlush stdout
  void $ runSpreadPriceFeed dbPath baseAsset $ runTimeIOFinal $ between1 asset period from to

-- hostName :: String
-- hostName = "127.0.0.1"

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

-- listToTQueue :: TQueue (Maybe a) -> [a] -> IO ()
-- listToTQueue queue xs = do
--   forM_ xs $
--     atomically
--       . writeTQueue queue
--       . Just
--   atomically $
--     writeTQueue queue Nothing
--
-- listFromTQueue :: TQueue (Maybe a) -> IO [a]
-- listFromTQueue queue = do
--   maybeElements <-
--     LazyIO.run $
--       forM (repeat ()) $
--         const $
--           LazyIO.interleave $
--             atomically $
--               readTQueue queue
--   return $ fromJust <$> takeWhile isJust maybeElements

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
