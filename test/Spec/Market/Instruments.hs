{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.Market.Instruments where

import Data.Either
import Data.Maybe
import Data.Record.Hom
import Data.Time
import Market
import Market.Instruments
import Market.Instruments.Test
import Market.Ops
import Market.Types
import Market.Types.Test
import Numeric.Algebra hiding ((>))
import Polysemy
import Polysemy.Input
import Test.Tasty
import Test.Tasty.QuickCheck hiding (tolerance)
import Test.Tasty.TH

type ThreeLabels = '["a", "b", "c"]
type Prcs = Prices ThreeLabels
type Port = Portfolio ThreeLabels
type BalCfg = BalanceConfig ThreeLabels ThreeLabels
type BalSt = BalanceState ThreeLabels ThreeLabels

test_Hold :: [TestTree]
test_Hold =
    [ testInstrumentLaws @ThreeLabels arbitraryConfig arbitraryConfig
    , testProperty "allocation is one point" onePointAllocation
    ] where
        onePointAllocation :: Prcs -> Property
        onePointAllocation prices = alloc === onePoint (labelIn @"a") where
            alloc = runInit prices (Hold #a) initAllocation

        arbitraryConfig = return $ Hold #a

test_Balance_Hold :: [TestTree]
test_Balance_Hold =
    [ testInstrumentLaws @ThreeLabels arbitraryApproxConfig arbitraryExactConfig
    , testInstantExact "portfolio is balanced (instant)"
        portfolioIsBalanced
    , testContinuousExact "portfolio is balanced (continuous)"
        portfolioIsBalanced
    -- TODO: no-op within updateEvery
    ] where
        portfolioIsBalanced = whenNotBroke do
            IConfig config <- input
            prices <- input
            portfolio' <- fst <$> runExecuteM execute
            let valueAlloc = fromJust $ valueAllocation prices portfolio'
            return $ property
                $ isBalanced (tolerance config) valueAlloc (target config)

        arbitraryApproxConfig = do
            tolerance <- arbitraryPositiveFraction
            updateEvery <- arbitrary `suchThat` \x -> x >= 0
            arbitraryConfig tolerance updateEvery

        arbitraryExactConfig = arbitraryConfig zero 0

        arbitraryConfig tolerance updateEvery = do
            target <- arbitrary
            return BalanceConfig
                { configs
                     = #a := someInstrumentConfig @ThreeLabels (Hold #a)
                    :& #b := someInstrumentConfig @ThreeLabels (Hold #b)
                    :& #c := someInstrumentConfig @ThreeLabels (Hold #c)
                    :& Empty
                , target = target
                , tolerance = tolerance
                , updateEvery = updateEvery
                }

        testInstantExact
            = testInstrumentPropertyInstant arbitraryExactConfig
        testContinuousExact
            = testInstrumentPropertyContinuous arbitraryExactConfig

tests :: TestTree
tests = $(testGroupGenerator)
