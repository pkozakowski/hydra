{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.Market.Simulation where

import Data.Coerce
import Data.Either
import Data.List
import Data.Maybe
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Record.Hom as HR
import Data.Time
import Market
import Market.Instruments
import Market.Ops
import Market.Simulation
import Market.Time
import Market.Types
import Market.Types.Test
import Numeric.Algebra hiding ((>))
import Numeric.Delta
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Output as Output
import Polysemy.State as State
import Prelude hiding ((/))
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
            $ runTimeConst time
            $ runMarketSimulation portfolio
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
    , testProperty "Balance changes portfolio <=> price ratios change"
        $ forAll (resize 5 arbitrary)
        $ balanceChangesPortfolioIffPriceRatiosChange
    ] where
        holdConservesPortfolio :: TimeSeries Prcs -> Port -> Property
        holdConservesPortfolio priceSeries initPortfolio
            = counterexample ("portfolios: " ++ show portfolios)
            $ n > 1 ==> length (nub portfolios) === 1 where
                n = length srs where TimeSeries srs = priceSeries
                portfolios :: [Port]
                portfolios = runBacktest priceSeries initPortfolio (Hold #a)

        balanceChangesPortfolioIffPriceRatiosChange
            :: TimeSeries Prcs -> Port -> Dist -> Property
        balanceChangesPortfolioIffPriceRatiosChange
            priceSeries initPortfolio target
            =   counterexample ("prices: "     ++ show prices    )
            $   counterexample ("portfolios: " ++ show portfolios)
            $   fullSupport target && totalValue initPrices initPortfolio > zero
            ==> changes priceRatios === changes portfolios where
                fullSupport (Distribution dist) = all (> Share zero) dist
                TimeSeries ((_, initPrices) :| _) = priceSeries
                changes (head :| tail)
                    = snd <$> scanl f (head, False) tail where
                        f (x, _) y = (y, x /= y)
                priceRatios = rebase <$> prices where
                    rebase (Prices prices) = rebaseOne <$> prices where
                        rebaseOne (Price x) = Price $ x / base where
                            Price base = HR.get #a prices
                prices = snd <$> srs where
                    TimeSeries srs = priceSeries
                portfolios
                    =  initPortfolio
                    :| runBacktest priceSeries initPortfolio config
                config = BalanceConfig
                    { configs
                         = #a := Hold #a
                        ~& #b := Hold #b
                        ~& #c := Hold #c
                        ~& noConfigs @ThreeLabels
                    , target = target
                    , tolerance = zero
                    , updateEvery = 0
                    }

        runBacktest priceSeries initPortfolio config
            = fromRight undefined $ run $ runError $ fmap fst $ runOutputList
            $ backtest priceSeries initPortfolio config
            $ Output.output =<< input @Port

tests :: TestTree
tests = $(testGroupGenerator)
