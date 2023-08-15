{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.Market.Feed.DB where

import Control.Exception
import Control.Monad.Logger
import Data.Bifunctor
import Data.Either
import Data.Map.Static
import Data.Maybe
import Data.Time (diffUTCTime)
import Data.Time.Clock
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Database.Persist.Sql hiding (Key)
import Database.Persist.Sqlite hiding (Key)
import Market
import Market.Feed
import Market.Feed.DB hiding (Key)
import Market.Feed.Dummy
import Market.Feed.Types
import Market.Time
import Polysemy
import Polysemy.Error
import Polysemy.Logging
import System.Directory
import System.IO.Error
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

type FixedF = StaticMap String FixedScalar
type ScalarF = StaticMap String Scalar

test_runFeedWithDBCache :: [TestTree]
test_runFeedWithDBCache =
  [ testCase "transparency @ second" $ transparency Second 100
  , testCase "transparency @ minute" $ transparency Minute 100
  ]
  where
    transparency period secs = do
      time <- runM $ runTimeIO now
      let runFeedToScalar
            :: Members [Time, Error String] r
            => Sem (Feed FixedF : r) (TimeSeries FixedF)
            -> Sem r (TimeSeries ScalarF)
          runFeedToScalar =
            mapFeedSeries (fmap (unpersistScalar @FixedScalar @Scalar)) . runFeedToFixed
          runFeedToFixed = runFeedDummy 2 7
          feed
            :: (FeedMap f, Key f ~ String, Members [Feed f, Time, Error String] r)
            => Sem r (TimeSeries f)
          feed =
            between
              ["abc", "def"]
              period
              (negate secs `addUTCTime` time)
              time
      let resultPure =
            run
              . runError
              . runTimeConst time
              . runFeedToScalar
              $ feed
      let runCachedTwice =
            fmap distribute
              . runFinal
              . runLogging
              . errorToIOFinal
              . embedToFinal
              . runTimeConst time
              . runFeedWithDBCache ":memory:" runFeedToFixed
              $ (,) <$> feed <*> feed
      -- first call populates the cache, second reads from it
      (resultCached1, resultCached2) <- runCachedTwice
      resultCached1 @?= resultPure
      resultCached2 @?= resultPure
      where
        distribute = \case
          Left e -> (Left e, Left e)
          Right (x, y) -> (Right x, Right y)

tests :: TestTree
tests = $(testGroupGenerator)
