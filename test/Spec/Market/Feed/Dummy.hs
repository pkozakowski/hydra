{-# LANGUAGE TemplateHaskell #-}

module Spec.Market.Feed.Dummy where

import Data.Either
import Data.List
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Static
import Data.Set qualified as Set
import Data.Time
import Market.Feed
import Market.Feed.Dummy
import Market.Feed.Ops (periodToSeconds)
import Market.Feed.Types
import Market.Time
import Market.Types
import Polysemy
import Polysemy.Error
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.TH

test_runFeedDummy :: [TestTree]
test_runFeedDummy =
  [ testProperty "between1 time advances by period" timeAdvancesByPeriod,
    testProperty "between1 values are not constant" valuesAreNotConstant,
    testProperty "between1 overlapping ranges agree" overlappingRangesAgree,
    testProperty "between1 different periods agree" differentPeriodsAgree,
    testProperty "between consistent with between1" $
      mapSize (const 10) betweenConsistentWithBetween1
  ]
  where
    timeAdvancesByPeriod ::
      Integer -> Integer -> String -> Period -> UTCTime -> Integer -> Property
    timeAdvancesByPeriod start modulo key period from numPeriods =
      numPeriodsOk numPeriods ==> fromTimeMatches .&&. timeDeltasMatch
      where
        fromTimeMatches = abs (head times `diffUTCTime` from) < periodDelta period
        timeDeltasMatch = conjoin $ (periodDelta period ===) <$> timeDeltas
        timeDeltas = zipWith diffUTCTime (tail times) times
        times = fmap fst $ seriesToList $ runFeedBetween1 start modulo key period from to
        to = advance period numPeriods from

    valuesAreNotConstant ::
      Integer -> Integer -> String -> UTCTime -> Integer -> Property
    valuesAreNotConstant start modulo key from numPeriods =
      numPeriodsOk numPeriods && modulo > 1 ==> length (nub values) > 1
      where
        values = fmap snd $ seriesToList $ runFeedBetween1 start modulo key Second from to
        to = advance Second numPeriods from

    overlappingRangesAgree ::
      Integer ->
      NonZero Integer ->
      String ->
      Period ->
      UTCTime ->
      Positive Integer ->
      Positive Integer ->
      Positive Integer ->
      Property
    overlappingRangesAgree
      start
      (NonZero modulo)
      key
      period
      from
      (Positive numPeriods1)
      (Positive numPeriods2)
      (Positive numPeriods3) =
        preconditions ==> fromMiddle2 `Set.intersection` middle1To == middle1Middle2
        where
          preconditions =
            all numPeriodsOk [numPeriods1, numPeriods2, numPeriods3] && modulo /= 0
          fromMiddle2 = seriesToSet $ runFeedBetween1 start modulo key period from middle2
          middle1To = seriesToSet $ runFeedBetween1 start modulo key period middle1 to
          middle1Middle2 = seriesToSet $ runFeedBetween1 start modulo key period middle1 middle2
          middle1 = advance period numPeriods1 from
          middle2 = advance period numPeriods2 middle1
          to = advance period numPeriods3 middle2

    differentPeriodsAgree ::
      Integer -> Integer -> String -> Period -> UTCTime -> Integer -> Property
    differentPeriodsAgree start modulo key periodLow from numPeriods =
      preconditions ==> seriesToSet seriesHigh `Set.isProperSubsetOf` seriesToSet seriesLow
      where
        preconditions = numPeriodsOk numPeriods && periodLow /= maxBound && modulo /= 0
        seriesLow = runFeedBetween1 start modulo key periodLow from to
        seriesHigh = runFeedBetween1 start modulo key periodHigh from to
        to = advance periodHigh numPeriods from
        periodHigh = succ periodLow

    betweenConsistentWithBetween1 ::
      Integer -> Integer -> NonEmpty String -> Period -> UTCTime -> Integer -> Property
    betweenConsistentWithBetween1 start modulo keys period from numPeriods =
      preconditions ==> conjoin (keyConsistent <$> NonEmpty.toList keys)
      where
        preconditions = modulo /= 0 && numPeriodsOk numPeriods
        mapSeries = runFeedBetween start modulo keys period from to
        singleSeries key = runFeedBetween1 start modulo key period from to
        keyConsistent key = fmap (! key) mapSeries === singleSeries key
        to = advance period numPeriods from

    seriesToSet = Set.fromList . seriesToList
    advance period n from = (fromInteger n * periodDelta period) `addUTCTime` from
    numPeriodsOk numPeriods = numPeriods >= 2 && numPeriods <= 100
    periodDelta period = fromInteger (periodToSeconds period)
    runFeedBetween start modulo keys period from to =
      runFeed start modulo to $
        between @(StaticMap String FixedScalar) keys period from to
    runFeedBetween1 start modulo key period from to =
      runFeed start modulo to $
        between1 @(StaticMap String FixedScalar) key period from to
    runFeed start modulo at =
      fromRight undefined
        . run
        . runError
        . runTimeConst at
        . runFeedDummy start modulo

tests :: TestTree
tests = $(testGroupGenerator)
