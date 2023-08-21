{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.Market.Simulation where

import Control.Monad
import Data.Bifunctor
import Data.Coerce
import Data.Either
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Static hiding (null)
import Data.Maybe
import Data.Ord
import Data.Time
import Market
import Market.Strategy.Balance
import Market.Strategy.Hold
import Market.Strategy.Some
import Market.Ops
import Market.Simulation
import Market.Time
import Market.Types
import Numeric.Algebra hiding ((<), (>))
import qualified Numeric.Algebra as Algebra
import Numeric.Delta
import Numeric.Precision
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Output as Output
import Polysemy.State as State
import Prelude hiding ((+), (/))
import Test.QuickCheck hiding (tolerance)
import Test.QuickCheck.Instances.Time
import Test.Tasty
import Test.Tasty.QuickCheck hiding (tolerance)
import Test.Tasty.TH

-- | Low-level tests using raw Market actions.
test_runMarketSimulation :: [TestTree]
test_runMarketSimulation =
    [ testProperty "trade succeeds iff sufficient balance"
        tradeSucceedsIffSufficientBalance
    , testProperty "trade succeeds with relative amount"
        tradeSucceedsWithRelativeAmount
    , testProperty "trade PortfolioDelta signs"
        tradePortfolioDeltaSigns
    , testProperty "trade subtracts absoluteAmount"
        tradeSubtractsAbsoluteAmount
    , testProperty "trade conserves totalValue at zero fees"
        tradeConservesTotalValueAtZeroFees
    , testProperty "trade final value decreases with fees"
        tradeFinalValueDecreasesWithFees
    ] where
        tradeSucceedsIffSufficientBalance
            :: UTCTime -> Fees -> Prices -> Portfolio -> Asset -> Asset
            -> Amount -> Property
        tradeSucceedsIffSufficientBalance
            time fees prices portfolio from to amount
                = label ("success: " ++ show actualSuccess)
                $ counterexample ("result: " ++ show result)
                $ actualSuccess === expectedSuccess
                    where
                        actualSuccess = isRight result
                        expectedSuccess
                            = from /= to
                           && portfolio ! from >= amount
                           && enoughForFees fees from orderAmount portfolio
                        result = runTrade
                            time fees prices portfolio from to orderAmount
                        orderAmount = Absolute amount

        tradeSucceedsWithRelativeAmount
            :: UTCTime -> Fees -> Prices -> Portfolio -> Asset -> Asset -> Share
            -> Property
        tradeSucceedsWithRelativeAmount time fees prices portfolio from to share
            = from /= to && enoughForFees fees from orderAmount portfolio
          ==> property
            $ isRight
            $ runTrade time fees prices portfolio from to orderAmount where
                orderAmount = Relative share

        tradePortfolioDeltaSigns
            :: UTCTime -> Fees -> Prices -> Portfolio -> Asset -> Asset
            -> OrderAmount -> Property
        tradePortfolioDeltaSigns time fees prices portfolio from to orderAmount
              = isRight result
            ==> all (uncurry checkSign) $ toList portfolioDelta where
                result = runTrade time fees prices portfolio from to orderAmount
                portfolio' = fromRight undefined result
                checkSign assetIn amountDelta =
                    if amountDelta < zero
                        then assetIn == from
                            || (fst <$> fixed fees) == Just assetIn
                    else if amountDelta > zero
                        then assetIn == to
                        else True
                portfolioDelta = portfolio' `delta` portfolio

        tradeSubtractsAbsoluteAmount
            :: UTCTime -> Fees -> Prices -> Portfolio -> Asset -> Asset
            -> OrderAmount -> Property
        tradeSubtractsAbsoluteAmount
                time fees prices portfolio from to orderAmount
            = isRight result ==> amount' === expectedAmount' where
                result = runTrade time fees prices portfolio from to orderAmount
                amount = portfolio ! from
                absAmount = absoluteAmount fees from amount orderAmount
                subtractAmount
                    = absAmount
                    + case fixed fees of
                        Just (feeAsset, feeAmount)
                            | feeAsset == from -> feeAmount
                            | otherwise -> zero
                        Nothing -> zero
                portfolio' = fromRight undefined result
                amount' = portfolio' ! from
                expectedAmount'
                    = fromJust $ amount `sigma` (zero `delta` subtractAmount)

        tradeConservesTotalValueAtZeroFees
            :: UTCTime -> Prices -> Portfolio -> Asset -> Asset -> OrderAmount
            -> Property
        tradeConservesTotalValueAtZeroFees
            time prices portfolio from to orderAmount
              = isRight result
            ==> totalValue prices portfolio' === totalValue prices portfolio
                where
                    result = runTrade
                        time zeroFees prices portfolio from to orderAmount
                    portfolio' = fromRight undefined result

        tradeFinalValueDecreasesWithFees
            :: UTCTime -> Fees -> Fees -> Prices -> Portfolio -> Asset -> Asset
            -> OrderAmount -> Property
        tradeFinalValueDecreasesWithFees
            time fees fees' prices portfolio from to orderAmount
                = isRight result && isRight result'
              ==> case fees `order` fees' of
                    Just ord
                        -> Down finalValue `compare` Down finalValue' == ord
                        || absAmount == zero
                    Nothing
                        -> discard
                where
                    [result, result'] =
                        [ runTrade time fs prices portfolio' from to orderAmount
                        | fs <- [fees, fees']
                        ]
                    [finalValue, finalValue']
                        = fmap (totalValue prices . fromRight undefined)
                        $ [result, result']
                    absAmount = absoluteAmount
                        fees from (portfolio' ! from) orderAmount
                    portfolio' = portfolio + (const (Amount one) `remap` prices)

        enoughForFees fees asset amount portfolio = case fixed fees of
            Just (feeAsset, feeAmount)
                -> portfolio ! feeAsset >= feeAmount
                && (feeAsset /= asset || balance >= absAmount + feeAmount) where
                    balance = portfolio ! asset
                    absAmount = absoluteAmount fees asset balance amount
            Nothing -> True

        runTrade time fees prices portfolio from to orderAmount
            = fmap fst
            $ run
            $ runError
            $ runInputConst @Prices prices
            $ runInputConst @Fees fees
            $ runTimeConst time
            $ runMarketSimulation portfolio
            $ trade from to orderAmount

{- |
High-level tests using Strategies.

They assume that:

    * backtest uses the same logic as runMarketSimulation in every step
    * runMarketSimulation works correctly
    * Hold and Balance work correctly with runMarketSimulation
-}
test_backtest :: [TestTree]
test_backtest =
    [ testProperty "Hold conserves portfolio" holdConservesPortfolio
    , testProperty "Balance changes portfolio <=> price ratios change"
        $ forAll (resize 3 arbitrary)
        $ balanceChangesPortfolioIffPriceRatiosChange
    ] where
        holdConservesPortfolio :: TimeSeries Prices -> Portfolio -> Property
        holdConservesPortfolio priceSeries initPortfolio
            = counterexample ("portfolios: " ++ show portfolios)
            $ n > 1 ==> length (nub portfolios) === 1 where
                n = length srs where TimeSeries srs = priceSeries
                portfolios :: [Portfolio]
                portfolios = runBacktest priceSeries initPortfolio $ Hold "A"

        balanceChangesPortfolioIffPriceRatiosChange
            :: TimeSeries Prices
            -> Portfolio
            -> Distribution StrategyName
            -> Property
        balanceChangesPortfolioIffPriceRatiosChange
            priceSeries initPortfolio target = property do
                priceSeries' <- disturb priceSeries
                let prices = snd <$> srs where
                        TimeSeries (_ :| srs) = priceSeries'
                    priceRatios = rebase <$> prices
                    TimeSeries ((_, initPrices) :| _) = priceSeries'
                    portfolios = runBacktest priceSeries' initPortfolio config
                return @Gen
                    $ counterexample ("prices: "     ++ show prices    )
                    $ counterexample ("portfolios: " ++ show portfolios)
                    $ collect (changes priceRatios)
                    $ fullSupport target
                   && totalValue initPrices initPortfolio > zero
                  ==> changes priceRatios === changes portfolios
                where
                    -- Randomly repeat prices to make the test case
                    -- distribution more interesting.
                    disturb (TimeSeries (head :| tail))
                        = TimeSeries . (head :|) . concat <$> forM tail \x -> do
                            n <- choose (0, 2)
                            return $ replicate n x

                    fullSupport (Distribution dist)
                        = all (> Share zero) dist
                       && size dist == length testStrategyNames

                    changes :: Eq a => [a] -> [Bool]
                    changes = discardWhenNull . \case
                        [] -> []
                        h : t -> tail $ snd <$> scanl f (h, False) t
                        where
                            discardWhenNull xs
                                = if null xs then discard else xs
                            f (x, _) y = (y, x /= y)

                    rebase (Prices prices) = rebaseOne <$> prices where
                        rebaseOne (Price x) = Price $ x / base where
                            Price base = prices ! head testAssets

                    config = BalanceConfig
                        { configs
                            = fromList
                            $ hold <$> zip testStrategyNames testAssets
                        , target = target
                        , tolerance = zero
                        , updateEvery = 0
                        } where
                            hold = second (someStrategyConfig . Hold)

        runBacktest
            :: Strategy c s
            => TimeSeries Prices
            -> Portfolio
            -> c
            -> [Portfolio]
        runBacktest priceSeries initPortfolio config
            = fromRight undefined
            $ run
            $ runError
            $ fmap fst
            $ runOutputList
            $ runPrecisionExact
            $ backtest zeroFees priceSeries initPortfolio config
            $ Output.output =<< input @Portfolio

tests :: TestTree
tests = $(testGroupGenerator)
