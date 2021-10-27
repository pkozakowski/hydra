{-# LANGUAGE TemplateHaskell #-}

module Market.Feed where

import qualified Data.List.NonEmpty as NonEmpty
import Data.Time
import Data.Time.Clock.POSIX
import Market.Time
import Market.Types
import Polysemy
import Polysemy.Error
import Prelude hiding (until)

data Feed v m a where
    Between' :: UTCTime -> UTCTime -> Feed v m (Maybe (TimeSeries v))

makeSem ''Feed

between
    :: Members [Feed v, Time, Error String] r
    => UTCTime -> UTCTime -> Sem r (TimeSeries v)
between from to = do
    time <- now
    let to' = min to time
    if from >= to' then
        throw $ showInterval from to ++ " is empty"
    else do
        maybeSeries <- between' from to'
        case maybeSeries of
            Just series -> return series
            Nothing -> throw $ "no datapoints in " ++ showInterval from to'
    where
        showInterval from to
            = "time interval " ++ show from ++ " .. " ++ show to

since
    :: Members [Feed v, Time, Error String] r
    => UTCTime -> Sem r (TimeSeries v)
since from = do
    time <- now
    between from time

until
    :: Members [Feed v, Time, Error String] r
    => UTCTime -> Sem r (TimeSeries v)
until to = do
    time <- now
    let from = posixSecondsToUTCTime 0
    between from $ min to time

ever
    :: Members [Feed v, Time, Error String] r
    => Sem r (TimeSeries v)
ever = until =<< now
