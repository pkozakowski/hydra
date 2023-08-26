{-# LANGUAGE RecordWildCards #-}

module Command.Sync where

import Control.Monad
import Data.List.NonEmpty (NonEmpty, nonEmpty)
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
import Market.Feed.Types
import Market.Ops
import Market.Time
import Numeric.Precision
import Options.Applicative hiding (UnexpectedError)
import Polysemy
import Polysemy.Error
import Polysemy.Logging
import System.IO
import Text.Printf (printf)

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
  let it assets to =
        runErrorWithLog
          ( fetchBatch
              dbPath
              Minute
              baseAsset
              (prev to)
              to
              assets
          )
          >>= \case
            Left (AssetPairNotFound ap) -> do
              let asset = numerator ap
              warning $ "asset " <> show asset <> " not found; continuing without it"
              maybe
                (throw "no more assets")
                pure
                $ nonEmpty
                $ NonEmpty.filter (/= asset) assets
            Left err -> throw $ show err
            Right _ -> pure assets
  foldM_ it (NonEmpty.fromList $ Asset <$> assets) $ iterate prev time
  embedFinal $ putStrLn ""

fetchBatch
  :: Members [Error FeedError, Logging, Final IO] r
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
  runPriceSpreadFeed dbPath baseAsset $ runTimeIOFinal $ between1_ asset period from to

showMinute :: UTCTime -> String
showMinute (UTCTime d t) = show d <> " " <> show0 h <> ":" <> show0 m <> " UTC"
  where
    TimeOfDay h m _ = timeToTimeOfDay t
    show0 = printf "%02d"
