{-# LANGUAGE TemplateHaskell #-}

module Spec.Market.Instrument.Balance where

import Data.Bifunctor
import Data.Either
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Static
import Data.Maybe
import Data.Time
import Market
import Market.Instrument.Balance
import Market.Instrument.Hold
import Market.Instrument.Ops
import Market.Instrument.Some
import Market.Instrument.Test
import Market.Ops
import Market.Simulation
import Numeric.Algebra hiding ((>), (+), (-), (/))
import Numeric.Precision
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Output as Output
import Test.Tasty
import Test.Tasty.QuickCheck hiding (tolerance)
import Test.Tasty.QuickCheck.Laws
import Test.Tasty.TH

test_Balance_Hold :: [TestTree]
test_Balance_Hold =
    [ testLaws $ instrumentLaws arbitraryApproxConfig arbitraryExactConfig
    , testProperty "portfolio is balanced (instant)"
        $ instrumentPropertyInstant
            arbitraryMatchingExactConfig (pure zeroFees) portfolioIsBalanced
    , testProperty "portfolio is balanced (continuous)"
        $ instrumentPropertyContinuous
            arbitraryMatchingExactConfig (pure zeroFees) portfolioIsBalanced
    , testProperty "rebalances are rare"
        $ forAll ((,) <$> arbitraryApproxConfig <*> resize 5 arbitrary)
        $ uncurry rebalancesAreRare
    ] where
        portfolioIsBalanced
            :: forall r
             . Members (ExecuteEffects BalanceConfig BalanceState) r
            => Sem r Property
        portfolioIsBalanced = whenNotBroke do
            IConfig config <- input @(IConfig BalanceConfig) @r
            prices <- input @Prices
            portfolio' <- fst <$> runExecuteM execute
            let valueAlloc = fromJust $ valueAllocation prices portfolio'
            return
                $ property
                $ isBalanced (tolerance config) valueAlloc
                $ mapDistKeys (Asset . unInstrumentName)
                $ target config

        rebalancesAreRare
            :: BalanceConfig -> Fees -> TimeSeries Prices -> Portfolio
            -> Property
        rebalancesAreRare config fees priceSeries initPortfolio
              = updateEvery config > 0
            ==> withMaxSuccess 10 prop where
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
                    = fromRight discard
                    $ run
                    $ runError
                    $ fmap fst
                    $ runOutputList
                    $ runPrecisionExact
                    $ backtest fees priceSeries initPortfolio config do
                        portfolio <- input @Portfolio
                        Output.output portfolio

        arbitraryApproxConfig = do
            tolerance <- arbitraryPositiveScalar
            updateEvery <- arbitrary `suchThat` \x -> x >= 0
            arbitraryConfig tolerance updateEvery

        arbitraryExactConfig = arbitraryConfig zero 0

        arbitraryConfig tolerance updateEvery = do
            instruments
               <- sequenceA
                $ fmap (const $ someInstrumentConfig . Hold <$> arbitrary)
                $ testInstrumentNames
            target <- arbitrary
            return BalanceConfig
                { configs
                    = fromList $ zip testInstrumentNames instruments
                , target = target
                , tolerance = tolerance
                , updateEvery = updateEvery
                }

        arbitraryMatchingExactConfig = do
            target <- mapDistKeys (InstrumentName . unAsset) <$> arbitrary
            return BalanceConfig
                { configs
                    = fromList
                    $ fmap
                        ( \asset
                         -> ( InstrumentName $ unAsset asset
                            , someInstrumentConfig $ Hold asset
                            )
                        )
                    $ testAssets
                , target = target
                , tolerance = zero
                , updateEvery = 0
                }
            
        mapKeys f = fromList . fmap (first f) . toList
        mapDistKeys f (Distribution d) = Distribution $ mapKeys f d

tests :: TestTree
tests = $(testGroupGenerator)