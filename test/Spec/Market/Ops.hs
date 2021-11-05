{-# LANGUAGE TemplateHaskell #-}

module Spec.Market.Ops where

import Data.Approx
import Data.Approx.Test
import Data.Bifunctor
import Data.List hiding (sum)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Static
import Data.Maybe
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Market.Ops
import Market.Types
import Numeric.Algebra hiding ((<), (>), sum)
import Numeric.Algebra.Test
import Prelude hiding ((+), (-), (*))
import qualified Prelude
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.TH

test_windows :: [TestTree]
test_windows = fmap (uncurry testProperty)
    [ ("number of windows", wrap numberOfWindows)
    , ("windows jump by stride", wrap windowsJumpByStride)
    , ("window contents", wrap windowContents)
    ] where
        numberOfWindows
            :: NominalDiffTime
            -> NominalDiffTime
            -> TimeSeries Int
            -> Property
        numberOfWindows windowLen stride series
            = actual === expected where
                actual
                    = NonEmpty.length
                    $ unTimeSeries
                    $ windows windowLen stride series
                expected
                    = ceiling (numStridesFrac windowLen stride series) + 1

        windowsJumpByStride
            :: NominalDiffTime
            -> NominalDiffTime
            -> TimeSeries Int
            -> Property
        windowsJumpByStride windowLen stride series@(TimeSeries txs)
            = actual === expected where
                actual
                    = NonEmpty.toList
                    $ fmap ((`diffUTCTime` begin) . fst)
                    $ unTimeSeries wnds where
                        begin = fst $ NonEmpty.head txs
                        wnds = windows windowLen stride series
                expected
                    = take (length actual)
                    $ (windowLen Prelude.+) <$> [0, stride ..]

        windowContents
            :: NominalDiffTime
            -> NominalDiffTime
            -> TimeSeries Int
            -> Property
        windowContents windowLen stride series@(TimeSeries txs)
            = conjoin $ NonEmpty.toList $ windowOk <$> wnds where
                windowOk (end, maybeWindow)
                    = txs' === NonEmpty.filter inside txs where
                        txs'
                            = maybe [] (NonEmpty.toList . unTimeSeries)
                            $ maybeWindow
                        inside (time, _) = begin <= time && time < end where
                            begin = Prelude.negate windowLen `addUTCTime` end
                TimeSeries wnds = windows windowLen stride series

        seriesLen series = end `diffUTCTime` begin where
            begin = fst $ NonEmpty.head $ unTimeSeries series
            end   = fst $ NonEmpty.last $ unTimeSeries series
        numStridesFrac windowLen stride series
            = max (seriesLen series Prelude.- windowLen) 0 Prelude./ stride

        wrap prop windowLen stride series
            = counterexample ("seriesLen: " ++ show (seriesLen series))
            $ counterexample ("numStridesFrac: " ++ show n)
            $ (windowLen >= stride && stride > 0 && n < 1000 ==>)
            $ prop windowLen stride series where
                n = numStridesFrac windowLen stride series

test_downsample :: [TestTree]
test_downsample = fmap (uncurry testProperty . second (mapSize $ const 10))
    [ ("downsampled <= original", downsampledIncludedInOriginal)
    , ("delta time >= period", deltaTimeGreaterEqualPeriod)
    , ("endpoints are within period", endpointsAreWithinPeriod)
    ] where
        downsampledIncludedInOriginal
            :: Positive NominalDiffTime -> TimeSeries Int -> Property
        downsampledIncludedInOriginal (Positive period) series
            = counterexample ("downsampled: " ++ show downsampled)
            $ testResampleSubsequence period downsampled series where
                downsampled = downsample period series

        deltaTimeGreaterEqualPeriod
            :: Positive NominalDiffTime -> TimeSeries Int -> Property
        deltaTimeGreaterEqualPeriod (Positive period) series
            = counterexample ("downsampled: " ++ show downsampled)
            $ compareDeltaTimePeriod (>=) period downsampled where
                downsampled = downsample period series

        endpointsAreWithinPeriod
            :: Positive NominalDiffTime -> TimeSeries Int -> Property
        endpointsAreWithinPeriod (Positive period) series
            = counterexample ("downsampled: " ++ show downsampled)
            $ testEndpointsWithin period series downsampled where
                downsampled = downsample period series

test_upsample :: [TestTree]
test_upsample = fmap (uncurry testProperty . second (mapSize $ const 10))
    [ ("original <= upsampled", originalIncludedInUpsampled)
    , ("delta time <= period", deltaTimeLessEqualPeriod)
    , ("endpoints are within period", endpointsAreWithinPeriod)
    , ("efficiency", efficiency)
    ] where
        originalIncludedInUpsampled
            :: Positive NominalDiffTime -> TimeSeries Int -> Property
        originalIncludedInUpsampled (Positive period) series
            = counterexample ("upsampled: " ++ show upsampled)
            $ testResampleSubsequence period series upsampled where
                upsampled = upsample period series

        deltaTimeLessEqualPeriod
            :: Positive NominalDiffTime -> TimeSeries Int -> Property
        deltaTimeLessEqualPeriod (Positive period) series
            = counterexample ("upsampled: " ++ show upsampled)
            $ compareDeltaTimePeriod (<=) period upsampled where
                upsampled = upsample period series

        endpointsAreWithinPeriod
            :: Positive NominalDiffTime -> TimeSeries Int -> Property
        endpointsAreWithinPeriod (Positive period) series
            = counterexample ("upsampled: " ++ show upsampled)
            $ testEndpointsWithin period series upsampled where
                upsampled = upsample period series

        efficiency
            :: Positive NominalDiffTime -> TimeSeries Int -> Property
        efficiency (Positive period) series
            = counterexample ("upsampled: " ++ show upsampled)
            $ length upsampled <= length series + maxNewPoints
            where
                upsampled = upsample period series
                maxNewPoints = floor ((end `diffUTCTime` begin) / period) where
                    (begin, end) = beginEndTimes series
                    (/) = (Prelude./)

testResampleSubsequence
    :: NominalDiffTime -> TimeSeries Int -> TimeSeries Int -> Property
testResampleSubsequence period subseries series
    = property $ subseries `isSubsequenceOf` series where
        downsampled = downsample period series
        isSubsequenceOf = isSubsequenceOfTS' eq where
            eq (t, x) (t', x')
              = abs (t `diffUTCTime` t') <= period && x == x'
            isSubsequenceOfTS' eq s s'
                = isSubsequenceOf' eq
                    (NonEmpty.toList $ unTimeSeries s)
                    (NonEmpty.toList $ unTimeSeries s') where
                        isSubsequenceOf' eq xs ys = case (xs, ys) of
                            ([], _) -> True
                            (_, []) -> False
                            (x : xs', y : ys)
                                | x `eq` y  -> isSubsequenceOf' eq xs' ys
                                | otherwise -> isSubsequenceOf' eq xs  ys

compareDeltaTimePeriod
    :: (NominalDiffTime -> NominalDiffTime -> Bool)
    -> NominalDiffTime -> TimeSeries Int -> Bool
compareDeltaTimePeriod rel period series
    = foldl (&&) True
    $ fmap snd
    $ maybe [] id
    $ fmap seriesToList
    $ convolve (\(t, _) (t', _) -> t' `diffUTCTime` t `rel` period)
    $ series

testEndpointsWithin
    :: NominalDiffTime -> TimeSeries Int -> TimeSeries Int -> Property
testEndpointsWithin period series series'
    = begin' `diffUTCTime` begin <= period
 .&&. end' `diffUTCTime` end <= period where
        (begin, end) = beginEndTimes series
        (begin', end') = beginEndTimes series'

beginEndTimes :: TimeSeries a -> (UTCTime, UTCTime)
beginEndTimes = ((,) <$> fst . head <*> fst . last) . seriesToList

test_convolve :: [TestTree]
test_convolve =
    [ testProperty "sum inverses difference" sumInversesDifference
    ] where
        sumInversesDifference :: TimeSeries Double -> Property
        sumInversesDifference series
              = isJust maybeDiffs
            ==> first Prelude.+ sum diffs ==~ last where
                TimeSeries ((_, first) :| _) = series
                last = snd $ NonEmpty.last $ unTimeSeries series
                maybeDiffs = convolve diff series where
                    diff (_, x) (_, x') = x' Prelude.- x
                diffs = fromJust maybeDiffs

test_convolveDilated :: [TestTree]
test_convolveDilated = fmap (uncurry testProperty . second (mapSize $ const 10))
    [ ("delta time >= period ==> convolveDilated == convolve", tooLong1)
    , ("end - begin < period ==> convolveDilated == Nothing", tooLong2)
    ] where
        tooLong1
            :: NominalDiffTime
            -> Fun (TimeStep Int, TimeStep Int) Int
            -> TimeSeries Int
            -> Property
        tooLong1 period f series
            = compareDeltaTimePeriod (>=) period series
          ==> convolveDilated period f' series == convolve f' series where
                f' = applyFun2 f

        tooLong2
            :: NominalDiffTime
            -> Fun (TimeStep Int, TimeStep Int) Int
            -> TimeSeries Int
            -> Property
        tooLong2 period f series
            = end `diffUTCTime` begin < period
          ==> convolveDilated period f' series' == Nothing where
                f' = applyFun2 f
                (begin, end) = beginEndTimes series'
                series' = TimeSeries $ first incFreq <$> unTimeSeries series
                incFreq = posixSecondsToUTCTime . (/ 10) . utcTimeToPOSIXSeconds
                (/) = (Prelude./)

test_integrate :: [TestTree]
test_integrate =
    [ testProperty "integral of constant" integralOfConstant
    ] where
        integralOfConstant :: Double -> TimeSeries () -> Property
        integralOfConstant constant times
            = integrate (const constant <$> times) ==~ constant

test_sweep :: [TestTree]
test_sweep = fmap (uncurry testProperty)
    [ ("completeness", completeness)
    , ("order", order)
    ] where
        completeness :: StaticMap Int (TimeSeries Int) -> Property
        completeness mapOfSeries = sort flatMap === sort flatEvents where
            flatMap = flattenEntry =<< toList mapOfLists where
                flattenEntry (k, tvs)
                    = (\(t, v) -> (t, k, v)) <$> tvs
            flatEvents = flattenEvent =<< sweep mapOfLists where
                flattenEvent (t, ev)
                    = (\(k, v) -> (t, k, v)) <$> NonEmpty.toList (changes ev)
            mapOfLists = seriesToList <$> mapOfSeries

        order :: StaticMap Int (TimeSeries Int) -> Property
        order mapOfSeries = events === sort events where
            events = sweep $ seriesToList <$> mapOfSeries

tests :: TestTree
tests = $(testGroupGenerator)
