{-# LANGUAGE TemplateHaskell #-}

module Spec.Market.Ops where

import Data.List
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Static
import Data.Time.Clock
import Market.Ops
import Market.Types
import Numeric.Algebra hiding ((<), (>))
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
