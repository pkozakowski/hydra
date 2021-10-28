{-# LANGUAGE TemplateHaskell #-}

module Spec.Market.Instrument.Ops where

import Data.Coerce
import Data.List
import Data.Map.Static
import Data.Maybe
import Market.Instrument.Ops
import Market.Types
import Numeric.Algebra hiding ((<), (>))
import Numeric.Delta
import Prelude hiding ((+), (-), (*))
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.TH

test_balancingTransfers_and_transferDelta :: [TestTree]
test_balancingTransfers_and_transferDelta = fmap (uncurry testProperty)
    [   ( "final Distribution is balanced"
        , wrap finalDistributionIsBalanced )
    ,   ( "final DistributionDelta sums to zero"
        , wrap finalDistributionDeltaSumsToZero )
    ,   ( "transfers are few"
        , wrap transfersAreFew )
    ] where
        finalDistributionIsBalanced
            :: Scalar -> Distribution Asset -> Distribution Asset -> Property
        finalDistributionIsBalanced tol current target
            = label (bucketScalar "tolerance" tol)
            $ isBalanced tol final target where
                final = fromJust $ current `sigma` delta where
                    delta = foldl (+) zero transferDeltas where
                        transferDeltas
                            = fmap transferDelta
                            $ balancingTransfers tol current target

        finalDistributionDeltaSumsToZero
            :: Scalar -> Distribution Asset -> Distribution Asset -> Property
        finalDistributionDeltaSumsToZero tol current target
            = foldl (+) zero diff === zero where
                DistributionDelta diff = foldl (+) zero transferDeltas where
                    transferDeltas
                        = fmap transferDelta
                        $ balancingTransfers tol current target

        transfersAreFew
            :: Scalar -> Distribution Asset -> Distribution Asset -> Property
        transfersAreFew tol current target
            = label (show len ++ " transfers")
            -- Worst-case optimal number of transfers is
            -- the number of assets - 1.
            $ len < limit where
                len
                    = length
                    $ balancingTransfers tol current target
                limit
                    = length
                    $ nub
                    $ fmap fst
                    $ toList current ++ toList target

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
            -> Asset
            -> Asset
            -> Distribution Asset
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
            -> Asset
            -> Asset
            -> Distribution Asset
            -> Property
        farImpliesUnbalanced tol tol' from to target@(Distribution targetMap)
              = labelTolerances tol tol'
              $ counterexample ("far: " ++ show far)
              $ nonTrivial
            ==> not $ isBalanced tol far target where
                far = fromJust $ target `sigma` transferDelta maxTr where
                    maxTr = maxTransfer tol' from to target
                nonTrivial
                     = from /= to
                    && targetMap ! from > Share zero
                    && targetMap ! to > Share zero

        maxTransfer tol from to target
            = ShareTransfer from to $ Share $ tol * maxChange where
                maxChange = min fromBal $ min toBal $ one - toBal
                fromBal = coerce $ targetMap ! from
                toBal = coerce $ targetMap ! to
                Distribution targetMap = target

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

bucketScalar :: String -> Scalar -> String
bucketScalar label scalar = label ++ " " ++
    if scalar == zero then "= 0"
    else if scalar < one then "in (0, 1)"
    else if scalar == one then "= 1"
    else "> 1"

tests :: TestTree
tests = $(testGroupGenerator)
