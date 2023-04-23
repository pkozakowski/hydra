{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Market.Feed.DB where

import Control.Monad
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Coerce
import Data.List
import Data.Map qualified as Map
import Data.Map.Class
import Data.Maybe
import Data.Proxy
import Data.Set qualified as Set
import Data.Time
import Data.Time.Clock.POSIX
import Data.Typeable
import Data.Vector (Vector)
import Data.Vector qualified as V
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite
import Database.Persist.TH
import Dhall (function)
import Language.Haskell.TH
import Market.Feed
import Market.Feed.DB.Types
import Market.Feed.Ops
import Market.Feed.Types
import Market.Time
import Market.Types
import Polysemy
import Polysemy.Error

-- timestamp = periodstamp * period
-- periodstamp = batchstamp * batchSize + moment

batchSize :: Int
batchSize = 100

type Batchstamp = Int

type Moment = Int

type ResourceType = String

type ResourceKey = String

Database.Persist.TH.share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
    FeedBatch
      period Period
      batchstamp Batchstamp
      type_ ResourceType
      key ResourceKey
      moments (Vector Moment)
      values (Vector PersistScalar)

      Primary period batchstamp type_ key
      FeedBatchUnique period batchstamp type_ key

      deriving Show
 |]

runFeedWithDBCache ::
  forall f r a.
  Members [Error String, Embed IO] r =>
  (forall a. Sem (Feed f : r) a -> Sem r a) ->
  Sem (Feed f : r) a ->
  Sem r a
runFeedWithDBCache lowerFeed = interpret \case
  Between1_ key period from to ->
    between1_UsingBetween_ (runFeedWithDBCache lowerFeed) key period from to
  Between_ (keys :: [k]) period from to -> do
    let type_ = show $ typeRep $ Proxy @f
        (fromBS, fromMoment) = splitTime period from
        (toBS, toMoment) = splitTime period to
    existingBatches <- selectBatchRange type_ (show <$> keys) period fromBS toBS
    let missingBatchstampKeys =
          determineMissingBatches (show <$> keys) period fromBS toBS existingBatches
        (completeBatches, incompleteBatchstampKeys) =
          partitionCompleteBatches fromBS fromMoment toBS toMoment existingBatches
    newBatches <-
      runFeedForBatches lowerFeed period $
        missingBatchstampKeys ++ incompleteBatchstampKeys
    upsertBatches newBatches
    return $ seriesFromList $ batchesToTimeSteps $ completeBatches ++ newBatches

selectBatchRange ::
  Members [Error String, Embed IO] r =>
  ResourceType ->
  [ResourceKey] ->
  Period ->
  Batchstamp ->
  Batchstamp ->
  Sem r [FeedBatch]
selectBatchRange type_ keys period fromBS toBS = do
  batches <-
    runQuery $
      selectList
        [ FeedBatchType_ ==. type_,
          FeedBatchKey <-. keys,
          FeedBatchPeriod ==. period,
          FeedBatchBatchstamp >=. fromBS,
          FeedBatchBatchstamp <=. toBS
        ]
        []
  return $ entityVal <$> batches

runQuery :: Members '[Embed IO] r => SqlPersistT (LoggingT IO) a -> Sem r a
runQuery query =
  embed $
    -- TODO: forward to our own logging
    -- TODO: parameterize the db location
    runStdoutLoggingT $
      withSqliteConn "squid.db" $
        \conn -> runSqlConn query conn

determineMissingBatches ::
  [ResourceKey] ->
  Period ->
  Batchstamp ->
  Batchstamp ->
  [FeedBatch] ->
  [(Batchstamp, ResourceKey)]
determineMissingBatches keys period fromBS toBS batches =
  let existingBatches =
        Set.fromList $
          [ (feedBatchBatchstamp, feedBatchKey)
            | FeedBatch {..} <- batches,
              feedBatchKey `elem` keys
          ]
      requestedKeys = Set.fromList keys
      requestedBatches = [(bs, k) | bs <- [fromBS .. toBS], k <- keys]
   in filter (\batch -> not $ Set.member batch existingBatches) requestedBatches

-- | Splits the input batches into 2 groups:
--   * complete batches, i.e. ones that have all of the moments falling into the given
--     (Batchstamp, Moment) interval
--   * incomplete batches, which are returned as (Batchstamp, ResourceKey) pairs.
partitionCompleteBatches ::
  Batchstamp ->
  Moment ->
  Batchstamp ->
  Moment ->
  [FeedBatch] ->
  ([FeedBatch], [(Batchstamp, ResourceKey)])
partitionCompleteBatches fromBS fromMoment toBS toMoment batches =
  (complete, strip <$> incomplete)
  where
    (complete, incomplete) = partition isComplete batches
    isComplete batch =
      filter inRange (V.toList $ feedBatchMoments batch) == [low .. high]
      where
        inRange moment = moment >= low && moment <= high
        low = if feedBatchBatchstamp batch == fromBS then fromMoment else 0
        high = if feedBatchBatchstamp batch == toBS then toMoment else batchSize - 1
    strip FeedBatch {..} = (feedBatchBatchstamp, feedBatchKey)

selectBatches ::
  Members [Error String, Embed IO] r =>
  ResourceType ->
  Period ->
  [(Batchstamp, ResourceKey)] ->
  Sem r [FeedBatch]
selectBatches type_ period batchstampKeys = do
  let batchFilters =
        map
          ( \(bs, k) ->
              [ FeedBatchPeriod ==. period,
                FeedBatchBatchstamp ==. bs,
                FeedBatchType_ ==. type_,
                FeedBatchKey ==. k
              ]
          )
          batchstampKeys
      orFilters = foldr1 (\f1 f2 -> f1 ++ [FilterOr f2]) batchFilters
  batches <- runQuery $ selectList orFilters []
  return $ map entityVal batches

runFeedForBatches ::
  forall f k v r a.
  ( FeedMap k v f,
    Members [Error String, Embed IO] r
  ) =>
  (forall a. Sem (Feed f : r) a -> Sem r a) ->
  Period ->
  [(Batchstamp, ResourceKey)] ->
  Sem r [FeedBatch]
runFeedForBatches lowerFeed period =
  fmap catMaybes . mapM (runFeedForBatch lowerFeed period)

runFeedForBatch ::
  forall f k v r a.
  ( FeedMap k v f,
    Members [Error String, Embed IO] r
  ) =>
  (forall a. Sem (Feed f : r) a -> Sem r a) ->
  Period ->
  (Batchstamp, ResourceKey) ->
  Sem r (Maybe FeedBatch)
runFeedForBatch lowerFeed period (batchstamp, key) = do
  let from = joinTime period batchstamp 0
      to = joinTime period (batchstamp + 1) 0
  mTimeSeries <- lowerFeed $ between1_ @f @k (read key) period from to
  let momentsValues = case mTimeSeries of
        Nothing -> []
        Just timeSeries ->
          let timeSteps = seriesToList timeSeries
           in map
                ( \(t, v) ->
                    (snd $ splitTime period t, coerce @v @PersistScalar v)
                )
                timeSteps
  return case momentsValues of
    [] -> Nothing
    _ ->
      let (moments, values) = V.unzip $ V.fromList momentsValues
       in Just $
            FeedBatch
              period
              batchstamp
              (show $ typeRep $ Proxy @f)
              key
              moments
              values

upsertBatches ::
  Members [Error String, Embed IO] r =>
  [FeedBatch] ->
  Sem r ()
upsertBatches = mapM_ upsertBatch
  where
    upsertBatch batch = do
      let key =
            FeedBatchUnique
              (feedBatchPeriod batch)
              (feedBatchBatchstamp batch)
              (feedBatchType_ batch)
              (feedBatchKey batch)
      runQuery $
        upsert
          batch
          [ FeedBatchMoments =. feedBatchMoments batch,
            FeedBatchValues =. feedBatchValues batch
          ]

batchesToTimeSteps ::
  ( Read k,
    Coercible PersistScalar v,
    BuildMap k v f
  ) =>
  [FeedBatch] ->
  [TimeStep f]
batchesToTimeSteps batches =
  let batchstampMomentKeys =
        Map.fromListWith
          (++)
          [ (feedBatchBatchstamp b, [(feedBatchKey b, m, v)])
            | b <- batches,
              (m, v) <-
                zip
                  (V.toList $ feedBatchMoments b)
                  (V.toList $ feedBatchValues b)
          ]
      batchstampToFs batchstamp keyMomentValues =
        Map.toList $
          fromList
            <$> Map.fromListWith
              (++)
              [ (joinTime period batchstamp m, [(read k, coerce v)])
                | (k, m, v) <- keyMomentValues
              ]
      period = feedBatchPeriod (head batches)
   in concatMap (uncurry batchstampToFs) (Map.toList batchstampMomentKeys)

splitTime :: Period -> UTCTime -> (Batchstamp, Moment)
splitTime period time = (batchstamp, moment)
  where
    periodSeconds = periodToSeconds period
    periodstamp = floor $ utcTimeToPOSIXSeconds time / fromIntegral periodSeconds
    (batchstamp, moment) = periodstamp `divMod` batchSize

joinTime :: Period -> Batchstamp -> Moment -> UTCTime
joinTime period batchstamp moment = posixSecondsToUTCTime (realToFrac posixSeconds)
  where
    periodSeconds = periodToSeconds period
    posixSeconds =
      fromIntegral (batchstamp * batchSize + moment) * fromIntegral periodSeconds
