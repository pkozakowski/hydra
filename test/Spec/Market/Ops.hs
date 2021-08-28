{-# LANGUAGE TemplateHaskell #-}

module Spec.Market.Ops where

import Data.Coerce
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe
import Data.Record.Hom
import Data.Time.Clock
import Market.Ops
import Market.Types
import Market.Types.Test
import Numeric.Algebra hiding ((<), (>))
import Numeric.Algebra.Test
import Numeric.Delta
import Prelude hiding ((+), (-), (*))
import qualified Prelude
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.TH

type ThreeLabels = '["a", "b", "c"]
type Dist3 = Distribution ThreeLabels
type DistDelta3 = DistributionDelta ThreeLabels

type FiveLabels = '["a", "b", "c", "d", "e"]
type Dist5 = Distribution FiveLabels
type DistDelta5 = DistributionDelta FiveLabels

test_balancingTransfers_and_transferDelta :: [TestTree]
test_balancingTransfers_and_transferDelta = fmap (uncurry testProperty)
    [   ( "final Distribution is balanced"
        , wrap finalDistributionIsBalanced )
    ,   ( "final DistributionDelta sums to zero"
        , wrap finalDistributionDeltaSumsToZero )
    ,   ( "transfers are few 3"
        , wrap transfersAreFew3 )
    ] ++ fmap (uncurry testProperty)
    [   ( "transfers are few 5"
        , wrap transfersAreFew5 )
    ] where
        finalDistributionIsBalanced :: Scalar -> Dist3 -> Dist3 -> Property
        finalDistributionIsBalanced tol current target
            = label (bucketScalar "tolerance" tol)
            $ isBalanced tol final target where
                final = fromJust $ current `sigma` delta where
                    delta = foldl (+) zero transferDeltas where
                        transferDeltas
                            = fmap transferDelta
                            $ balancingTransfers tol current target

        finalDistributionDeltaSumsToZero :: Scalar -> Dist3 -> Dist3 -> Property
        finalDistributionDeltaSumsToZero tol current target
            = foldl (+) zero diff === zero where
                DistributionDelta diff = foldl (+) zero transferDeltas where
                    transferDeltas
                        = fmap transferDelta
                        $ balancingTransfers tol current target

        transfersAreFew3 :: Scalar -> Dist3 -> Dist3 -> Property
        transfersAreFew3 tol current target
            = label (show len ++ " transfers")
            -- Worst-case optimal number of transfers is
            -- the number of assets - 1.
            $ len <= 2 where
                len = length $ balancingTransfers tol current target

        transfersAreFew5 :: Scalar -> Dist5 -> Dist5 -> Property
        transfersAreFew5 tol current target
            = label (show len ++ " transfers")
            $ len <= 4 where
                len = length $ balancingTransfers tol current target

        wrap prop current target
             = forAll (arbitrary `suchThat` \tol -> tol >= zero)
             $ \tol
            -- Time limit to detect infinite recursion.
            -> within 1000000
             $ prop tol current target

test_isBalanced :: [TestTree]
test_isBalanced = fmap (uncurry testProperty)
    [ ("close => balanced", wrapToleranceRel (<=) closeImpliesBalanced)
    , ("far => unbalanced", wrapToleranceRel (<) farImpliesUnbalanced)
    ] where
        closeImpliesBalanced
            :: Scalar
            -> Scalar
            -> LabelIn ThreeLabels
            -> LabelIn ThreeLabels
            -> Dist3
            -> Property
        closeImpliesBalanced tol tol' from to target
            = labelTolerances tol tol'
            $ counterexample ("close: " ++ show close)
            $ isBalanced tol' close target where
                close = fromJust $ target `sigma` transferDelta maxTr where
                    maxTr = maxTransfer tol from to target

        farImpliesUnbalanced
            :: Scalar
            -> Scalar
            -> LabelIn ThreeLabels
            -> LabelIn ThreeLabels
            -> Dist3
            -> Property
        farImpliesUnbalanced tol tol' from to target@(Distribution targetRec)
              = labelTolerances tol tol'
              $ counterexample ("far: " ++ show far)
              $ nonTrivial
            ==> not $ isBalanced tol far target where
                far = fromJust $ target `sigma` transferDelta maxTr where
                    maxTr = maxTransfer tol' from to target
                nonTrivial
                     = from /= to
                    && getIn from targetRec > Share zero
                    && getIn to targetRec > Share zero

        maxTransfer tol from to target
            = ShareTransfer from to $ Share $ tol * maxChange where
                maxChange = min fromBal $ min toBal $ one - toBal
                fromBal = coerce $ getIn from targetRec
                toBal = coerce $ getIn to targetRec
                Distribution targetRec = target

        labelTolerances tol tol'
            = label
                 $ bucketScalar "tolerance" tol
                ++ ", "
                ++ bucketScalar "tolerance'" tol'

        wrapToleranceRel rel prop
            = forAll ((arbitrary :: Gen (Scalar, Scalar)) `suchThat` validate)
            $ uncurry prop where
                validate (tol, tol')
                    = zero <= tol && tol `rel` tol' && tol' <= one

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

bucketScalar :: String -> Scalar -> String
bucketScalar label scalar = label ++ " " ++
    if scalar == zero then "= 0"
    else if scalar < one then "in (0, 1)"
    else if scalar == one then "= 1"
    else "> 1"

tests :: TestTree
tests = $(testGroupGenerator)
