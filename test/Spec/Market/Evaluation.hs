{-# LANGUAGE TemplateHaskell #-}

module Spec.Market.Evaluation where

import Data.Approx.Test
import Data.List.NonEmpty as NonEmpty
import Data.Maybe
import Market.Evaluation
import Market.Types
import Market.Types.Test
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.TH

test_convolve :: [TestTree]
test_convolve =
    [ testProperty "sum inverses difference" sumInversesDifference
    ] where
        sumInversesDifference :: TimeSeries Double -> Property
        sumInversesDifference series
              = isJust maybeDiffs
            ==> first + sum diffs ==~ last where
                TimeSeries ((_, first) :| _) = series
                last = snd $ NonEmpty.last $ unTimeSeries series
                maybeDiffs = convolve diff series where
                    diff (_, x) (_, x') = x' - x
                diffs = fromJust maybeDiffs

test_integrate :: [TestTree]
test_integrate =
    [ testProperty "integral of constant" integralOfConstant
    ] where
        integralOfConstant :: Double -> TimeSeries () -> Property
        integralOfConstant constant times
            = integrate (const constant <$> times) ==~ constant

tests :: TestTree
tests = $(testGroupGenerator)
