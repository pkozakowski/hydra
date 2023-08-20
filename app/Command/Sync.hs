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
batchDuration = nominalDay

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
  let prev t = negate batchDuration `addUTCTime` t
  forM_ (iterate prev time) \to ->
    fetchBatch
      dbPath
      Minute
      baseAsset
      (prev to)
      to
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
    putStr $ show asset <> "/" <> show baseAsset <> " @ " <> showMinute to <> "  "
    hFlush stdout
  void $ runSpreadPriceFeed dbPath baseAsset $ runTimeIOFinal $ between1_ asset period from to

showMinute :: UTCTime -> String
showMinute (UTCTime d t) = show d <> " " <> show h <> ":" <> show m <> " UTC"
  where
    TimeOfDay h m _ = timeToTimeOfDay t
