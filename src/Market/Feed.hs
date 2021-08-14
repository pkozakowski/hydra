{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Market.Feed where

import qualified Data.List.NonEmpty as NonEmpty
import Data.Time
import Data.Time.Clock.POSIX
import Market.Time
import Market.Types
import Polysemy
import Prelude hiding (until)

data Feed v m a where
    Between' :: UTCTime -> UTCTime -> Feed v m (Maybe (TimeSeries v))

makeSem ''Feed

between
    :: Members [Feed v, Time] r
    => UTCTime -> UTCTime -> Sem r (Maybe (TimeSeries v))
between from to = do
    time <- now
    let to' = min to time
    if from >= to' then
        return Nothing
    else 
        between' from to'

since
    :: Members [Feed v, Time] r
    => UTCTime -> Sem r (Maybe (TimeSeries v))
since from = do
    time <- now
    between from time

until
    :: Members [Feed v, Time] r
    => UTCTime -> Sem r (Maybe (TimeSeries v))
until to = do
    time <- now
    let from = posixSecondsToUTCTime 0
    between' from $ min to time

ever
    :: Members [Feed v, Time] r
    => Sem r (Maybe (TimeSeries v))
ever = until =<< now
