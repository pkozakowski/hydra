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
    [ testInstrumentLaws @ThreeLabels $ return $ Hold #a
    , testInitAllocationLaws @ThreeLabels $ return $ Hold #a
    , testProperty "allocation is one point" onePointAllocation
    ] where
        onePointAllocation :: Prcs -> Property
        onePointAllocation prices = alloc === onePoint (labelIn @"a") where
            alloc = runInit prices (Hold #a) initAllocation

test_Balance_Hold :: [TestTree]
test_Balance_Hold =
    [ testInstrumentLaws @ThreeLabels arbitraryConfig
    , testInitAllocationLaws @ThreeLabels
        $ arbitraryConfigWithTolerance zero
    , testInstant "portfolio is balanced (instant)" portfolioIsBalanced
    ] where
        portfolioIsBalanced = whenNotBroke do
            IConfig config <- input
            prices <- input
            portfolio' <- fst <$> runExecuteM execute
            let valueAlloc = fromJust $ valueAllocation prices portfolio'
            return $ property
                $ isBalanced (tolerance config) valueAlloc (target config)

        arbitraryConfig
            = arbitraryConfigWithTolerance =<< arbitraryPositiveFraction

        arbitraryConfigWithTolerance tolerance = do
            target <- arbitrary
            updateEvery <- arbitrary `suchThat` \x -> x > 0
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

        testInstant = testInstrumentPropertyInstant arbitraryConfig

tests :: TestTree
tests = $(testGroupGenerator)
