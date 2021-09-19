{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Market.Feed.MongoDB where

import Control.Exception
import Control.Logging
import Control.Monad
import Data.Aeson.Types
import Data.AesonBson
import Data.Bson
import Data.Constraint
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe
import Data.Proxy
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Text (Text, pack)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Type.Equality
import Data.Void
import Database.MongoDB
import GHC.Generics
import GHC.OverloadedLabels
import GHC.TypeLits as TL
import Market.Feed
import Market.Internal.IO
import Market.Ops
import Market.Types
import Polysemy
import Polysemy.Error hiding (throw, catch)
import qualified System.IO.Lazy as LazyIO

db :: Text
db = "hydra"

class FeedType t where
    feedName :: Text

class Period p where
    periodName :: Text
    numSeconds :: Integer
    type BatchBy p :: *

data Level p where
    Lowest :: Level Second
    Lower :: BatchablePeriod p' => Level p' -> Level p

class (Period p, Period (BatchBy p)) => BatchablePeriod p where
    level :: Level p

data Second

instance Period Second where
    periodName = "second"
    numSeconds = 1
    type BatchBy Second = Hour

instance BatchablePeriod Second where
    level = Lowest

data Minute

instance Period Minute where
    periodName = "minute"
    numSeconds = 60
    type BatchBy Minute = Day

instance BatchablePeriod Minute where
    level = Lower $ level @Second

data Hour

instance Period Hour where
    periodName = "hour"
    numSeconds = 60 * numSeconds @Minute
    type BatchBy Hour = Month

instance BatchablePeriod Hour where
    level = Lower $ level @Minute

data Day

instance Period Day where
    periodName = "day"
    numSeconds = 24 * numSeconds @Hour
    type BatchBy Day = Void

data Month

instance Period Month where
    periodName = "month"
    numSeconds = round $ 30.44 * fromIntegral (numSeconds @Day)
    type BatchBy Month = Void

data Batch ft atp byp = Batch
    { token :: String
    , batchstamp :: Int
    , moments :: Vector Int
    , values :: Vector Double
    } deriving (Show, Generic, FromJSON, ToJSON)

collection
    :: forall ft atp byp
     . (FeedType ft, Period atp, Period byp)
    => Text
collection
    = feedName @ft
   <> "_at_" <> periodName @atp
   <> "_by_" <> periodName @byp

query
    :: forall ft atp byp s
     . (FeedType ft, Period atp, Period byp, Select s)
    => String -> Int -> s
query token batchstamp = select
    [ "token" =: token
    , "batchstamp" =: batchstamp
    ] $ collection @ft @atp @byp

periodDiffTime :: forall p. Period p => NominalDiffTime
periodDiffTime = fromInteger $ numSeconds @p

timeToBatchstamp :: forall p. Period p => UTCTime -> Int
timeToBatchstamp = floor . (/ periodDiffTime @p) . utcTimeToPOSIXSeconds

batchstampToTime :: forall p. Period p => Int -> UTCTime
batchstampToTime = posixSecondsToUTCTime . (* periodDiffTime @p) . fromIntegral

timeToMoment :: forall atp byp. (Period atp, Period byp) => UTCTime -> Int
timeToMoment
    = fromInteger
    . (`mod` (numSeconds @byp `div` numSeconds @atp))
    . (`div` numSeconds @atp)
    . floor
    . utcTimeToPOSIXSeconds

momentToDiffTime :: forall p. Period p => Int -> NominalDiffTime
momentToDiffTime = (* periodDiffTime @p) . fromIntegral

mongo :: String -> Action IO a -> IO a
mongo hostName action = bracket (connect $ host hostName) close
    \pipe -> access pipe master db action

type SemInterpreter
     = forall r a
     . Members [Error String, Embed IO] r
    => String
    -> Sem (Feed Double : r) a
    -> Sem r a

type IOInterpreter
     = String
    -> UTCTime
    -> UTCTime
    -> IO (Maybe (TimeSeries Double))

getCachedBatch
    :: forall ft atp byp
     . (FeedType ft, Period atp, Period byp)
    => String -> String -> Int -> IO (Maybe (Batch ft atp byp))
getCachedBatch hostName token batchstamp = do
    maybeDocument <- mongo hostName
        $ findOne
        $ query @ft @atp @byp token batchstamp
    case maybeDocument of
        Just document -> either fail return
            $ parseEither parseJSON
            $ Object
            $ aesonify document
        Nothing -> return Nothing

cachedFetchBatch
    :: forall ft atp byp
     . (FeedType ft, Period atp, Period byp)
    => IOInterpreter
    -> String
    -> String
    -> Int
    -> IO (Maybe (TimeSeries Double))
cachedFetchBatch interpreter hostName token batchstamp' = do
    maybeBatch <- getCachedBatch @ft @atp @byp hostName token batchstamp'
    case maybeBatch of
        Just batch -> do
            debug $ batchText <> " found in MongoDB cache"
            return $ unpackBatch batch
        Nothing -> do
            debug $ batchText <> " not found in MongoDB cache; fetching"
            maybeSeries <- interpreter token beginTime endTime
            let maybeDownsampledSeries
                    = downsample (periodDiffTime @atp) <$> maybeSeries
            time <- getCurrentTime
            when (time >= endTime) do
                debug $ batchText <> " has already passed; adding to cache"
                let bson
                        = bsonifyBound
                        $ valueToObject
                        $ toJSON
                        $ packBatch maybeDownsampledSeries
                (void $ mongo hostName $ insert (collection @ft @atp @byp) bson)
                    `catch` \(e :: Failure) -> do
                        warn
                            $ "error while inserting document:\n"
                           <> pack (show bson)
                        throw e
            return maybeDownsampledSeries
    where
        batchText
            = "token "
           <> pack token
           <> ": "
           <> periodName @byp
           <> " starting at "
           <> pack (show beginTime)

        valueToObject (Object o) = o
        beginTime = batchstampToTime @byp batchstamp'
        endTime = batchstampToTime @byp $ batchstamp' + 1

        packBatch maybeSeries
            = Batch
                { token = token
                , batchstamp = batchstamp'
                , moments = moments
                , values = values
                } where
                    (moments, values)
                        = Vector.unzip
                        $ fmap fromTimeStep
                        $ Vector.filter (isBetween . fst)
                        $ Vector.fromList
                        $ timeSteps
                        where
                            fromTimeStep (time, value)
                                = (timeToMoment @atp @byp time, value)
                            isBetween time = beginTime <= time && time < endTime
                            timeSteps
                                = maybe [] NonEmpty.toList
                                $ unTimeSeries <$> maybeSeries

        unpackBatch batch
            = fmap TimeSeries
            $ nonEmpty
            $ fmap toTimeStep
            $ Vector.toList
            $ Vector.zip <$> moments <*> values
            $ batch where
                toTimeStep (moment, value) = (time, value) where
                    time = momentToDiffTime @atp moment `addUTCTime` beginTime
                beginTime = batchstampToTime @byp $ batchstamp batch

cachedFeedLevel
    :: forall ft atp
     . (FeedType ft, BatchablePeriod atp)
    => Level atp
    -> IOInterpreter
    -> String
    -> String
    -> UTCTime
    -> UTCTime
    -> IO (Maybe (TimeSeries Double))
cachedFeedLevel level secondInterpreter hostName token from to = do
    -- Fetch the data hour-by-hour going through the cache.
    fmap ((filterBetween =<<) . concatSeries . catMaybes)
        $ LazyIO.run
        $ forM [fromBS .. toBS]
        $ LazyIO.interleave
        . cachedFetchBatch
            @ft @atp @(BatchBy atp) interpreter hostName token
    where
        filterBetween
            = fmap TimeSeries
            . nonEmpty
            . NonEmpty.filter isBetween
            . unTimeSeries where
                isBetween (time, _) = from <= time && time < to
        concatSeries
            = fmap (TimeSeries . join . fmap unTimeSeries)
            . nonEmpty
        fromBS = timeToBatchstamp @(BatchBy atp) from
        toBS = timeToBatchstamp @(BatchBy atp) to
        interpreter = case level of
            Lowest -> secondInterpreter
            Lower level' -> cachedFeedLevel
                @ft level' secondInterpreter hostName

runFeedWithMongoCache
    :: forall ft atp r a
     .  ( FeedType ft
        , BatchablePeriod atp
        , Members [Error String, Embed IO] r
        )
    => String
    -> SemInterpreter
    -> String
    -> Sem (Feed Double : r) a
    -> Sem r a
runFeedWithMongoCache hostName secondInterpreter token = interpret \case
    Between' from to
        -> getBeginTime from to >>= \case
            Nothing -> return Nothing
            Just beginTime
               -> ioToSem
                $ withStderrLogging
                $ cachedFeedLevel
                    @ft (level @atp) secondIOInterpreter hostName token from' to
                        where
                            secondIOInterpreter token'' from'' to''
                                = semToIO @String
                                $ secondInterpreter token''
                                $ between' @Double from'' to''
                            from' = max from beginTime
        where
            getBeginTime from to = do
                hasHour
                   <- fmap isJust
                    $ ioToSem
                    $ getCachedBatch @ft @atp @(BatchBy atp) hostName token
                    $ timeToBatchstamp @(BatchBy atp) from
                if hasHour then
                    return $ Just from
                else
                    -- Prefetch the first datapoint to know where to begin in
                    -- case of broad queries. This takes advantage of lazy IO,
                    -- so we don't fetch the entire interval at once.
                    secondInterpreter token (between' @Double from to) >>= \case
                        Nothing
                            -> return Nothing
                        Just (TimeSeries ((beginTime, _) :| _))
                            -> return $ Just beginTime

cacheCoverage
    :: forall ft r a
     .  ( FeedType ft
        , Members [Error String, Embed IO] r
        )
    => String
    -> String
    -> Sem r (Maybe (UTCTime, UTCTime))
cacheCoverage hostName token = ioToSem $ withStderrLogging do
    maybeFirstHour <- mongo hostName $ findOne query
    case maybeFirstHour of
        Nothing -> return Nothing
        Just firstHour -> do
            batchstamps <- getBatchstampsSince $ batchstamp firstHour
            return
                $ Just
                ( batchstampToTime @(BatchBy Second) $ batchstamp firstHour
                , batchstampToTime @(BatchBy Second) $ last batchstamps + 1
                )
    where
        selection
            = select [ "token" =: token ]
            $ collection @ft @Second @(BatchBy Second)
        query = selection
            { sort = [ "batchstamp" =: (1 :: Int) ]
            , project = [ "batchstamp" =: (1 :: Int) ]
            }
        getBatchstampsSince batchstamp
            = getCachedBatch
                @ft @Second @(BatchBy Second)
                hostName token batchstamp >>= \case
                    Nothing -> return []
                    Just _ -> do
                        rest <- getBatchstampsSince $ batchstamp + 1
                        return $ batchstamp : rest
        batchstamp = fromInt32 . valueAt "batchstamp"
        fromInt32 (Int32 x) = fromIntegral x
