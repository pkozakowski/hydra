{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.Market.Instruments where

import Data.Either
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import Data.Record.Hom
import Data.Time
import Market
import Market.Instruments
import Market.Instruments.Test
import Market.Ops
import Market.Simulation
import Market.Types
import Market.Types.Test
import Numeric.Algebra hiding ((>), (+), (-), (/))
import Numeric.Precision
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Output as Output
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
    , testProperty "rebalances are rare"
        $ forAll ((,) <$> arbitraryApproxConfig <*> resize 5 arbitrary)
        $ uncurry rebalancesAreRare
    ] where
        portfolioIsBalanced = whenNotBroke do
            IConfig config <- input
            prices <- input
            portfolio' <- fst <$> runExecuteM execute
            let valueAlloc = fromJust $ valueAllocation prices portfolio'
            return $ property
                $ isBalanced (tolerance config) valueAlloc (target config)

        rebalancesAreRare
            :: BalCfg -> TimeSeries Prcs -> Port -> Property
        rebalancesAreRare config priceSeries initPortfolio
              = totalValue initPrices initPortfolio > zero
             && updateEvery config > 0
            ==> prop where
                prop = numRebalances <= limit where
                    limit = fromEnum (diff / updateEvery config) + 1 where
                        diff = diffUTCTime endTime beginTime
                numRebalances
                    = length (nubAdjacent $ initPortfolio : portfolios) where
                        nubAdjacent xs
                            = fmap snd $ nubBy eqAdjacent $ zip [0 ..] xs where
                                eqAdjacent (i, x) (j, y)
                                    = x == y && abs (i - j) <= 1
                TimeSeries ((beginTime, initPrices) :| rest) = priceSeries
                endTime
                    | rest == [] = beginTime
                    | otherwise  = fst $ last rest
                portfolios
                    = either (error . show) id
                    $ run
                    $ runError
                    $ fmap fst
                    $ runOutputList
                    $ runPrecisionExact
                    $ backtest zeroFees priceSeries initPortfolio config do
                        portfolio <- input @(Portfolio ThreeLabels)
                        Output.output portfolio

        arbitraryApproxConfig = do
            tolerance <- arbitraryPositiveFraction
            updateEvery <- arbitrary `suchThat` \x -> x >= 0
            arbitraryConfig tolerance updateEvery

        arbitraryExactConfig = arbitraryConfig zero 0

        arbitraryConfig tolerance updateEvery = do
            target <- arbitrary
            return BalanceConfig
                { configs
                     = #a := Hold #a
                    ~& #b := Hold #b
                    ~& #c := Hold #c
                    ~& noConfigs @ThreeLabels
                , target = target
                , tolerance = tolerance
                , updateEvery = updateEvery
                }

        testContinuousApprox
            = testInstrumentPropertyContinuous
                @ThreeLabels arbitraryApproxConfig
        testInstantExact
            = testInstrumentPropertyInstant @ThreeLabels arbitraryExactConfig
        testContinuousExact
            = testInstrumentPropertyContinuous @ThreeLabels arbitraryExactConfig

tests :: TestTree
tests = $(testGroupGenerator)
