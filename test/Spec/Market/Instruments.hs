{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
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
import Test.Tasty
import Test.Tasty.QuickCheck hiding (tolerance)
import Test.Tasty.TH

type ThreeLabels = '["a", "b", "c"]
type Prcs = Prices ThreeLabels
type Port = Portfolio ThreeLabels

test_Hold :: [TestTree]
test_Hold =
    [ testInstrumentLaws @ThreeLabels $ return $ Hold #a
    , testInitAllocationExecuteAgreement @ThreeLabels $ return $ Hold #a
    , testProperty "allocation is one point" onePointAllocation
    ] where
        onePointAllocation :: Prcs -> Property
        onePointAllocation prices = alloc === onePoint (labelIn @"a") where
            alloc = initAllocation prices $ Hold #a

test_Balance_Hold :: [TestTree]
test_Balance_Hold =
    [ testInstrumentLaws @ThreeLabels arbitraryConfig
    , testInitAllocationExecuteAgreement @ThreeLabels
        $ arbitraryConfigWithTolerance zero
    , testProperty "portfolio is balanced"
        $ forAll arbitraryConfig portfolioIsBalanced
    ] where
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

portfolioIsBalanced
    :: BalanceConfig ThreeLabels ThreeLabels -> UTCTime -> Prcs -> Port
    -> Property
portfolioIsBalanced config time prices portfolio
    =   totalValue prices portfolio > zero
    ==> isBalanced (tolerance config) valueAlloc (target config) where
        valueAlloc = fromJust $ valueAllocation prices portfolio' where
            portfolio'
                = fromRight undefined $ runExecute config time prices portfolio

tests :: TestTree
tests = $(testGroupGenerator)
