{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.Market.Simulation where

import Data.Coerce
import Data.Either
import Data.List
import Data.Maybe
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Record.Hom
import Data.Time
import Market
import Market.Instruments
import Market.Ops
import Market.Simulation
import Market.Types
import Market.Types.Test
import Numeric.Algebra hiding ((>))
import Numeric.Delta
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Output as Output
import Polysemy.State as State
import Test.QuickCheck hiding (tolerance)
import Test.QuickCheck.Instances.Time
import Test.Tasty
import Test.Tasty.QuickCheck hiding (tolerance)
import Test.Tasty.TH

type ThreeLabels = '["a", "b", "c"]
type Prcs = Prices ThreeLabels
type Port = Portfolio ThreeLabels
type Dist = Distribution ThreeLabels
type AssetIn = LabelIn ThreeLabels

-- | Low-level tests using raw Market actions.
test_runMarketSimulation :: [TestTree]
test_runMarketSimulation =
    [ testProperty "trade succeeds iff sufficient balance" tradeSucceedsIffSufficientBalance
    , testProperty "trade succeeds with relative amount" tradeSucceedsWithRelativeAmount
    , testProperty "trade PortfolioDelta signs" tradePortfolioDeltaSigns
    , testProperty "trade subtracts absoluteAmount" tradeSubtractsExactAmount
    , testProperty "trade conserves totalValue" tradeConservesTotalValue
    ] where
        tradeSucceedsIffSufficientBalance
            :: UTCTime -> Prcs -> Port -> AssetIn -> AssetIn -> Amount
            -> Property
        tradeSucceedsIffSufficientBalance time prices portfolio from to amount
            = success === (getIn from portfolio >= amount) where
            success
                = isRight $ runTrade time prices portfolio from to
                $ Absolute amount

        tradeSucceedsWithRelativeAmount
            :: UTCTime -> Prcs -> Port -> AssetIn -> AssetIn -> Share
            -> Property
        tradeSucceedsWithRelativeAmount time prices portfolio from to share
            = property $ isRight $ runTrade time prices portfolio from to
            $ Relative share

        tradePortfolioDeltaSigns
            :: UTCTime -> Prcs -> Port -> AssetIn -> AssetIn -> OrderAmount
            -> Property
        tradePortfolioDeltaSigns time prices portfolio from to orderAmount
            =   amount >= absoluteAmount amount orderAmount
            ==> all (uncurry checkSign) $ toList portfolioDelta where
            amount = getIn from portfolio
            portfolio'
                = fromRight undefined
                $ runTrade time prices portfolio from to orderAmount
            checkSign assetIn amountDelta =
                if assetIn == from then amountDelta <= zero
                else if assetIn == to then amountDelta >= zero
                else amountDelta == zero
            portfolioDelta = portfolio' `delta` portfolio

        tradeSubtractsExactAmount
            :: UTCTime -> Prcs -> Port -> AssetIn -> AssetIn -> OrderAmount
            -> Property
        tradeSubtractsExactAmount time prices portfolio from to orderAmount
            =   amount >= absAmount && from /= to
            ==> amount' === expectedAmount' where
            amount = getIn from portfolio
            absAmount = absoluteAmount amount orderAmount
            portfolio'
                = fromRight undefined
                $ runTrade time prices portfolio from to orderAmount
            amount' = getIn from portfolio'
            expectedAmount' = fromJust $ amount `sigma` (zero `delta` absAmount)

        tradeConservesTotalValue
            :: UTCTime -> Prcs -> Port -> AssetIn -> AssetIn -> OrderAmount
            -> Property
        tradeConservesTotalValue time prices portfolio from to orderAmount
            =   amount >= absoluteAmount amount orderAmount
            ==> totalValue prices portfolio' === totalValue prices portfolio where
                amount = getIn from portfolio
                portfolio'
                    = fromRight undefined
                    $ runTrade time prices portfolio from to orderAmount

        runTrade time prices portfolio from to orderAmount
            = fmap fst $ run $ runError $ runInputConst prices
            $ runMarketSimulation time portfolio
            $ trade from to orderAmount

{- |
High-level tests using Instruments.

They assume that:

    * backtest uses the same logic as runMarketSimulation in every step
    * runMarketSimulation works correctly
    * Hold and Balance work correctly with runMarketSimulation
-}
test_backtest :: [TestTree]
test_backtest =
    [ testProperty "Hold conserves portfolio" holdConservesPortfolio
    , testProperty "Balance changes portfolio <=> price changes"
        $ forAll (resize 5 arbitrary)
        $ balanceChangesPortfolioIffPriceChanges
    ] where
        holdConservesPortfolio :: TimeSeries Prcs -> Port -> Property
        holdConservesPortfolio priceSeries initPortfolio
            = counterexample ("portfolios: " ++ show portfolios)
            $ n > 1 ==> length (nub portfolios) === 1 where
                n = length srs where TimeSeries srs = priceSeries
                portfolios :: [Port]
                portfolios = runBacktest priceSeries initPortfolio (Hold #a)

        balanceChangesPortfolioIffPriceChanges
            :: TimeSeries Prcs -> Port -> Dist -> Property
        balanceChangesPortfolioIffPriceChanges priceSeries initPortfolio target
            =   fullSupport target && totalValue initPrices initPortfolio > zero
            ==> changes prices === changes portfolios where
                fullSupport (Distribution dist) = all (> Share zero) dist
                TimeSeries ((_, initPrices) :| _) = priceSeries
                changes (head :| tail)
                    = snd <$> scanl f (head, False) tail where
                        f (x, _) y = (y, x /= y)
                prices = snd <$> srs where
                    TimeSeries srs = priceSeries
                portfolios
                    =  initPortfolio
                    :| runBacktest priceSeries initPortfolio config
                config = BalanceConfig
                    { configs
                         = #a := someInstrumentConfig @ThreeLabels (Hold #a)
                        :& #b := someInstrumentConfig @ThreeLabels (Hold #b)
                        :& #c := someInstrumentConfig @ThreeLabels (Hold #c)
                        :& Empty
                    , target = target
                    , tolerance = zero
                    , updateEvery = 0
                    }

        runBacktest priceSeries initPortfolio config
            = fromRight undefined $ run $ runError $ fmap fst $ runOutputList
            $ backtest outputPortfolio priceSeries initPortfolio config where
                outputPortfolio = Output.output =<< input @Port

tests :: TestTree
tests = $(testGroupGenerator)
