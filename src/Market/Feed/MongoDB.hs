{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Market.Feed.MongoDB where

import Control.Exception
import Control.Logging
import Control.Monad
import Data.Aeson.Types
import Data.AesonBson
import Data.Bson
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Text (Text, pack)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Database.MongoDB
import GHC.Generics
import Market.Feed
import Market.Internal.IO
import Market.Types
import Polysemy
import Polysemy.Error hiding (throw, catch)
import qualified System.IO.Lazy as LazyIO

db :: Text
db = "hydra"

priceCollection :: Text
priceCollection = "price"

priceQuery :: Select s => String -> Int -> s
priceQuery token hourstamp = select
    [ "token" =: token
    , "hourstamp" =: hourstamp
    ] priceCollection

data PriceHourly = PriceHourly
    { token :: String
    , hourstamp :: Int
    , seconds :: Vector Int
    , prices :: Vector Double
    } deriving (Show, Generic, FromJSON, ToJSON)

timeToHourstamp :: UTCTime -> Int
timeToHourstamp = (`div` 3600) . floor . utcTimeToPOSIXSeconds

hourstampToTime :: Int -> UTCTime
hourstampToTime = posixSecondsToUTCTime . fromInteger . (* 3600) . fromIntegral

mongo :: String -> Action IO a -> IO a
mongo hostName action = bracket (connect $ host hostName) close
    \pipe -> access pipe master db action

type TokenFeedInterpreter
     = forall r a
     . Members [Error String, Embed IO] r
    => String
    -> Sem (Feed Double : r) a
    -> Sem r a

getCachedHourBSON :: String -> String -> Int -> IO (Maybe Document)
getCachedHourBSON hostName token
    = mongo hostName . findOne . priceQuery token

cachedFetchHourPrices
    :: String
    -> String
    -> TokenFeedInterpreter
    -> Int
    -> IO (Maybe (TimeSeries Double))
cachedFetchHourPrices hostName token interpreter hourstamp = do
    maybeHourlyBSON <- getCachedHourBSON hostName token hourstamp
    case maybeHourlyBSON of
        Just hourlyBSON -> do
            debug $ hourText <> " found in MongoDB cache"
            fmap unpackPrices
                $ either fail return
                $ parseEither parseJSON
                $ Object
                $ aesonify hourlyBSON
        Nothing -> do
            debug $ hourText <> " not found in MongoDB cache; fetching"
            hourPrices
               <- semToIO
                $ interpreter token
                $ between' beginTime endTime
            time <- getCurrentTime
            when (time >= endTime) do
                debug $ hourText <> " has already passed; adding to cache"
                let bson
                        = bsonifyBound
                        $ valueToObject
                        $ toJSON
                        $ packPrices hourPrices
                (void $ mongo hostName $ insert priceCollection bson)
                    `catch` \(e :: Failure) -> do
                        warn
                            $ "error while inserting document:\n"
                           <> pack (show bson)
                        throw e
            return hourPrices
    where
        hourText
             = "token "
            <> pack token
            <> ": hour starting at "
            <> pack (show beginTime)

        valueToObject (Object o) = o
        beginTime = hourstampToTime hourstamp
        endTime = hourstampToTime $ hourstamp + 1

        packPrices maybeSeries
            = PriceHourly
                { token = token
                , hourstamp = hourstamp
                , seconds = seconds
                , prices = prices
                } where
                    (seconds, prices)
                        = Vector.unzip
                        $ fmap fromTimeStep
                        $ Vector.filter (isBetween . fst)
                        $ Vector.fromList
                        $ timeSteps
                        where
                            fromTimeStep (time, price)
                                = (second, price) where
                                    second = floor posix `mod` 3600 where
                                        posix = utcTimeToPOSIXSeconds time
                            isBetween time = beginTime <= time && time < endTime
                            timeSteps
                                = maybe [] NonEmpty.toList
                                $ unTimeSeries <$> maybeSeries

        unpackPrices hourly
            = fmap TimeSeries
            $ nonEmpty
            $ fmap toTimeStep
            $ Vector.toList
            $ Vector.zip <$> seconds <*> prices
            $ hourly where
                toTimeStep (second, price) = (time, price) where
                    time = fromIntegral second `addUTCTime` beginTime

runPriceFeedWithMongoCache
    :: Members [Error String, Embed IO] r
    => String
    -> TokenFeedInterpreter
    -> String
    -> Sem (Feed Double : r) a
    -> Sem r a
runPriceFeedWithMongoCache hostName interpreter token = interpret \case
    Between' from to
        -> getBeginTime from to >>= \case
            Nothing -> return Nothing
            Just beginTime
                -> ioToSem $ withStderrLogging do
                    let from' = max from beginTime
                    -- Fetch the data hour-by-hour going through the cache.
                    fmap ((filterBetween =<<) . concatSeries . catMaybes)
                        $ LazyIO.run
                        $ forM [timeToHourstamp from' .. timeToHourstamp to]
                        $ LazyIO.interleave
                        . cachedFetchHourPrices hostName token interpreter
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
        where
            getBeginTime from to = do
                hasHour
                   <- fmap isJust
                    $ ioToSem
                    $ getCachedHourBSON hostName token
                    $ timeToHourstamp from
                if hasHour then
                    return $ Just from
                else
                    -- Prefetch the first datapoint to know where to begin in
                    -- case of broad queries. This takes advantage of lazy IO,
                    -- so we don't fetch the entire interval at once.
                    interpreter token (between' @Double from to) >>= \case
                        Nothing
                            -> return Nothing
                        Just (TimeSeries ((beginTime, _) :| _))
                            -> return $ Just beginTime

cacheCoverage
    :: Members [Error String, Embed IO] r
    => String
    -> String
    -> Sem r (Maybe (UTCTime, UTCTime))
cacheCoverage hostName token = ioToSem $ withStderrLogging do
    maybeFirstHour <- mongo hostName $ findOne query
    case maybeFirstHour of
        Nothing -> return Nothing
        Just firstHour -> do
            hourstamps <- getHourstampsSince $ hourstamp firstHour
            return
                $ Just
                ( hourstampToTime $ hourstamp firstHour
                , hourstampToTime $ last hourstamps + 1
                )
    where
        selection = select [ "token" =: token ] priceCollection
        query = selection
            { sort = [ "hourstamp" =: (1 :: Int) ]
            , project = [ "hourstamp" =: (1 :: Int) ]
            }
        getHourstampsSince hourstamp = do
            getCachedHourBSON hostName token hourstamp >>= \case
                Nothing -> return []
                Just _ -> do
                    rest <- getHourstampsSince $ hourstamp + 1
                    return $ hourstamp : rest
        hourstamp = fromInt32 . valueAt "hourstamp"
        fromInt32 (Int32 x) = fromIntegral x
