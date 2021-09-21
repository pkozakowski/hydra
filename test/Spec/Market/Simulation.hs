{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.Market.Simulation where

import Control.Monad
import Data.Coerce
import Data.Either
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe
import Data.Ord
import Data.Record.Hom as HR
import Data.Time
import Market
import Market.Instruments
import Market.Ops
import Market.Simulation
import Market.Time
import Market.Types
import Market.Types.Test
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

type FiveLabels = '["a", "b", "c", "d", "e"]
type Prcs = Prices FiveLabels
type Port = Portfolio FiveLabels
type Dist = Distribution FiveLabels
type Fs = Fees FiveLabels
type AssetIn = LabelIn FiveLabels

zeroFees :: Fees FiveLabels
zeroFees = Fees
    { fixed = Nothing
    , variable = zero
    }

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
            :: UTCTime -> Fs -> Prcs -> Port -> AssetIn -> AssetIn -> Amount
            -> Property
        tradeSucceedsIffSufficientBalance
            time fees prices portfolio from to amount
              = counterexample ("result: " ++ show result)
              $ actualSuccess === expectedSuccess
                where
                    actualSuccess = isRight result
                    expectedSuccess
                        = from /= to
                       && getIn from portfolio >= amount
                       && enoughForFees fees from orderAmount portfolio
                    result = runTrade
                        time fees prices portfolio from to orderAmount
                    orderAmount = Absolute amount

        tradeSucceedsWithRelativeAmount
            :: UTCTime -> Fs -> Prcs -> Port -> AssetIn -> AssetIn -> Share
            -> Property
        tradeSucceedsWithRelativeAmount time fees prices portfolio from to share
            = from /= to && enoughForFees fees from orderAmount portfolio
          ==> property
            $ isRight
            $ runTrade time fees prices portfolio from to orderAmount where
                orderAmount = Relative share

        tradePortfolioDeltaSigns
            :: UTCTime -> Fs -> Prcs -> Port -> AssetIn -> AssetIn
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
            :: UTCTime -> Fs -> Prcs -> Port -> AssetIn -> AssetIn
            -> OrderAmount -> Property
        tradeSubtractsAbsoluteAmount
                time fees prices portfolio from to orderAmount
            = isRight result ==> amount' === expectedAmount' where
                result = runTrade time fees prices portfolio from to orderAmount
                amount = getIn from portfolio
                absAmount = absoluteAmount amount orderAmount
                subtractAmount
                    = absAmount
                    + case fixed fees of
                        Just (feeAsset, feeAmount)
                            | feeAsset == from -> feeAmount
                            | otherwise -> zero
                        Nothing -> zero
                portfolio' = fromRight undefined result
                amount' = getIn from portfolio'
                expectedAmount'
                    = fromJust $ amount `sigma` (zero `delta` subtractAmount)

        tradeConservesTotalValueAtZeroFees
            :: UTCTime -> Prcs -> Port -> AssetIn -> AssetIn -> OrderAmount
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
            :: UTCTime -> Fs -> Fs -> Prcs -> Port -> AssetIn -> AssetIn
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
                        [ runTrade time fs prices portfolio from to orderAmount
                        | fs <- [fees, fees']
                        ]
                    [finalValue, finalValue']
                        = fmap (totalValue prices . fromRight undefined)
                        $ [result, result']
                    absAmount
                        = absoluteAmount (getIn from portfolio) orderAmount

        enoughForFees fees asset amount portfolio = case fixed fees of
            Just (feeAsset, feeAmount)
                -> getIn feeAsset portfolio >= feeAmount
                && (feeAsset /= asset || balance >= absAmount + feeAmount) where
                    balance = getIn asset portfolio
                    absAmount = absoluteAmount balance amount
            Nothing -> True

        runTrade time fees prices portfolio from to orderAmount
            = fmap fst
            $ run
            $ runError
            $ runInputConst @Prcs prices
            $ runInputConst @Fs fees
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
        $ forAll (resize 3 arbitrary)
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

                    fullSupport (Distribution dist) = all (> Share zero) dist

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
                            Price base = HR.get #a prices
                    config = BalanceConfig
                        { configs
                             = #a := Hold #a
                            ~& #b := Hold #b
                            ~& #c := Hold #c
                            ~& #d := Hold #d
                            ~& #e := Hold #e
                            ~& noConfigs @FiveLabels
                        , target = target
                        , tolerance = zero
                        , updateEvery = 0
                        }

        runBacktest
            :: Instrument FiveLabels c s
            => TimeSeries Prcs
            -> Port
            -> c
            -> [Port]
        runBacktest priceSeries initPortfolio config
            = fromRight undefined
            $ run
            $ runError
            $ fmap fst
            $ runOutputList
            $ runPrecisionExact
            $ backtest zeroFees priceSeries initPortfolio config
            $ Output.output =<< input @Port

tests :: TestTree
tests = $(testGroupGenerator)
