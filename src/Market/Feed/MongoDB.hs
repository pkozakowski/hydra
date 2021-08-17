{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Market.Feed.MongoDB where

import Control.Exception
import Control.Logging
import Control.Monad
import Data.Aeson.Types
import Data.AesonBson
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
import Market.Feed.Types
import Market.Internal.IO
import Market.Types
import Polysemy
import Polysemy.Error
import qualified System.IO.Lazy as LazyIO

db :: Text
db = "hydra"

priceVolumeCollection :: Text
priceVolumeCollection = "price_volume"

data PriceVolumeHourly = PriceVolumeHourly
    { token :: String
    , hourstamp :: Int
    , seconds :: Vector Int
    , prices :: Vector Double
    , volumes :: Vector Double
    } deriving (Show, Generic, FromJSON, ToJSON)

timeToHourstamp :: UTCTime -> Int
timeToHourstamp = (`div` 3600) . floor . utcTimeToPOSIXSeconds

hourstampToTime :: Int -> UTCTime
hourstampToTime = posixSecondsToUTCTime . fromInteger . (* 3600) . fromIntegral

mongo :: String -> Action IO a -> IO a
mongo hostName action = bracket (connect $ host hostName) close
    \pipe -> access pipe master db action

type PriceVolumeInterpreter
     = forall r a
     . Members [Error String, Embed IO] r
    => String
    -> Sem (Feed PriceVolume : r) a
    -> Sem r a

cachedFetchHourPriceVolumes
    :: String
    -> String
    -> PriceVolumeInterpreter
    -> Int
    -> IO (Maybe (TimeSeries PriceVolume))
cachedFetchHourPriceVolumes hostName token interpreter hourstamp = do
    maybeHourlyBSON <- mongo hostName $ findOne query
    case maybeHourlyBSON of
        Just hourlyBSON -> do
            debug $ hourText <> " found in MongoDB cache"
            fmap unpackPriceVolumes
                $ either fail return
                $ parseEither parseJSON
                $ Object
                $ aesonify hourlyBSON
        Nothing -> do
            debug $ hourText <> " not found in MongoDB cache; fetching"
            hourPriceVolumes
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
                        $ packPriceVolumes hourPriceVolumes
                void $ mongo hostName $ insert priceVolumeCollection bson
            return hourPriceVolumes
    where
        query = select 
            [ "token" =: token
            , "hourstamp" =: hourstamp
            ] priceVolumeCollection
        hourText
             = "token "
            <> pack token
            <> ": hour starting at "
            <> pack (show beginTime)
        valueToObject (Object o) = o
        beginTime = hourstampToTime hourstamp
        endTime = hourstampToTime $ hourstamp + 1
        packPriceVolumes maybeSeries
            = PriceVolumeHourly
                { token = token
                , hourstamp = hourstamp
                , seconds = seconds
                , prices = prices
                , volumes = volumes
                } where
                    (seconds, prices, volumes)
                        = Vector.unzip3
                        $ fmap fromTimeStep
                        $ Vector.filter (isBetween . fst)
                        $ Vector.fromList
                        $ timeSteps
                        where
                            fromTimeStep (time, pv)
                                = (second, price pv, volume pv) where
                                    second = floor posix `mod` 3600 where
                                        posix = utcTimeToPOSIXSeconds time
                            isBetween time = beginTime <= time && time < endTime
                            timeSteps
                                = maybe [] NonEmpty.toList
                                $ unTimeSeries <$> maybeSeries
        unpackPriceVolumes hourly
            = fmap TimeSeries
            $ nonEmpty
            $ fmap toTimeStep
            $ Vector.toList
            $ Vector.zip3 <$> seconds <*> prices <*> volumes
            $ hourly where
                toTimeStep (second, price, volume) =
                    ( time
                    , PriceVolume {price = price, volume = volume}
                    ) where
                        time = fromIntegral second `addUTCTime` beginTime

runPriceVolumeFeedWithMongoCache
    :: Members [Error String, Embed IO] r
    => String
    -> PriceVolumeInterpreter
    -> String
    -> Sem (Feed PriceVolume : r) a
    -> Sem r a
runPriceVolumeFeedWithMongoCache hostName interpreter token = interpret \case
    Between' from to
        -- Prefetch the first datapoint to know where to begin in case of broad
        -- queries. This takes advantage of lazy IO, so we don't fetch the
        -- entire interval at once.
        -> interpreter token (between' @PriceVolume from to) >>= \case
            Nothing -> return Nothing
            Just (TimeSeries ((beginTime, _) :| _))
                -> ioToSem $ withStderrLogging do
                    let from' = max from beginTime
                    -- Fetch the data hour-by-hour going through the cache.
                    fmap ((filterBetween =<<) . concatSeries . catMaybes)
                        $ LazyIO.run
                        $ forM [timeToHourstamp from' .. timeToHourstamp to]
                        $ LazyIO.interleave
                        . cachedFetchHourPriceVolumes hostName token interpreter
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
