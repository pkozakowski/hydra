{-# LANGUAGE MultiWayIf #-}
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
import Database.Persist.Sql
import Database.Persist.Sqlite
import Market
import Market.Feed
import Market.Feed.DB
import Market.Feed.Dummy
import Market.Feed.Types
import Market.Log
import Market.Time
import Polysemy
import Polysemy.Error
import System.Directory
import System.IO.Error
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

type F = StaticMap String Scalar

test_runFeedWithDBCache :: [TestTree]
test_runFeedWithDBCache =
  [ testCase "transparency @ second" $ transparency Second 100
  , testCase "transparency @ minute" $ transparency Minute 100
  ]
  where
    transparency period secs = do
      time <- runM $ runTimeIO now
      let runFeed = runFeedDummy 2 7
          feed :: Members [Feed F, Time, Error String] r => Sem r (TimeSeries F)
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
              . runFeed
              $ feed
      let runCachedTwice =
            fmap distribute
              . runFinal
              . runLog
              . errorToIOFinal
              . embedToFinal
              . runTimeConst time
              . runFeedWithDBCache ":memory:" runFeed
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
