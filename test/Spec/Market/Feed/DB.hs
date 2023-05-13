{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.Market.Feed.DB where

import Control.Exception
import Control.Monad.Logger
import Data.Either
import Data.Map.Static
import Data.Time (diffUTCTime)
import Data.Time.Clock
import Database.Persist.Sql
import Database.Persist.Sqlite
import Market
import Market.Feed
import Market.Feed.DB
import Market.Feed.Dummy
import Market.Feed.Types
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
  [ testCase "transparency @ second" $ transparency Second
  -- TODO: use a temporary file for the DB and then uncomment
  -- , testCase "transparency @ minute" $ transparency Minute
  ]
  where
    transparency period = do
      removeFile "squid.db"
        `Control.Exception.catch` \e ->
          if isDoesNotExistError e
            then return ()
            else throwIO e
      runStdoutLoggingT
        . withSqliteConn "squid.db"
        . runSqlConn
        $ runMigration migrateAll
      let test = do
            time <- runM $ runTimeIO now
            let runFeed = runFeedDummy 2 5
                feed :: Members [Feed F, Time, Error String] r => Sem r (TimeSeries F)
                feed =
                  between
                    ["abc", "def"]
                    period
                    (negate 10 `addUTCTime` time)
                    time
            let resultPure =
                  run
                    . runError
                    . runTimeConst time
                    . runFeed
                    $ feed
            let runCached =
                  runFinal
                    . errorToIOFinal
                    . embedToFinal
                    . runTimeConst time
                    $ runFeedWithDBCache runFeed feed
            -- first call populates the cache
            resultCached1 <- runCached
            resultCached1 @?= resultPure
            -- second reads from it
            resultCached2 <- runCached
            resultCached2 @?= resultPure
      test `Control.Exception.finally` removeFile "squid.db"

tests :: TestTree
tests = $(testGroupGenerator)
