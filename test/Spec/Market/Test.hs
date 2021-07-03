{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Spec.Market.Test where

import Data.Coerce
import Data.Either
import Data.Maybe
import Data.Record.Hom
import Market
import Market.Test
import Market.Types
import Market.Types.Test
import Numeric.Algebra
import Numeric.Delta
import Polysemy
import Polysemy.Error
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.TH

type ThreeLabels = '["a", "b", "c"]
type Prcs = Prices ThreeLabels
type Port = Portfolio ThreeLabels
type AssetIn = LabelIn ThreeLabels

test_runMarketMock :: [TestTree]
test_runMarketMock =
    [ testProperty "trade succeeds iff sufficient balance" tradeSucceedsIffSufficientBalance
    , testProperty "trade succeeds with relative amount" tradeSucceedsWithRelativeAmount
    , testProperty "trade PortfolioDelta signs" tradePortfolioDeltaSigns
    , testProperty "trade subtracts absoluteAmount" tradeSubtractsExactAmount
    , testProperty "trade conserves totalValue" tradeConservesTotalValue
    ] where
        tradeSucceedsIffSufficientBalance
            :: Prcs -> Port -> AssetIn -> AssetIn -> Amount -> Property
        tradeSucceedsIffSufficientBalance prices portfolio from to amount
            = success === (getIn from portfolio >= amount) where
            success
                = isRight $ runTrade prices portfolio from to $ Absolute amount

        tradeSucceedsWithRelativeAmount :: Prcs -> Port -> AssetIn -> AssetIn -> Share -> Property
        tradeSucceedsWithRelativeAmount prices portfolio from to share
            = property $ isRight $ runTrade prices portfolio from to $ Relative share

        tradePortfolioDeltaSigns :: Prcs -> Port -> AssetIn -> AssetIn -> OrderAmount -> Property
        tradePortfolioDeltaSigns prices portfolio from to orderAmount
            =   amount >= absoluteAmount amount orderAmount
            ==> all (uncurry checkSign) $ toList portfolioDelta where
            amount = getIn from portfolio
            portfolio'
                = fromRight undefined $ runTrade prices portfolio from to orderAmount
            checkSign assetIn amountDelta =
                if assetIn == from then amountDelta <= zero
                else if assetIn == to then amountDelta >= zero
                else amountDelta == zero
            portfolioDelta = portfolio' `delta` portfolio

        tradeSubtractsExactAmount :: Prcs -> Port -> AssetIn -> AssetIn -> OrderAmount -> Property
        tradeSubtractsExactAmount prices portfolio from to orderAmount
            =   amount >= absAmount && from /= to
            ==> amount' === expectedAmount' where
            amount = getIn from portfolio
            absAmount = absoluteAmount amount orderAmount
            portfolio' = fromRight undefined $ runTrade prices portfolio from to orderAmount
            amount' = getIn from portfolio'
            expectedAmount' = fromJust $ amount `sigma` (zero `delta` absAmount)

        tradeConservesTotalValue :: Prcs -> Port -> AssetIn -> AssetIn -> OrderAmount -> Property
        tradeConservesTotalValue prices portfolio from to orderAmount
            =   amount >= absoluteAmount amount orderAmount
            ==> totalValue prices portfolio' === totalValue prices portfolio where
                amount = getIn from portfolio
                portfolio' = fromRight undefined $ runTrade prices portfolio from to orderAmount

        runTrade prices portfolio from to orderAmount
            = run $ runError $ runMarketMock prices portfolio $ trade from to orderAmount

tests :: TestTree
tests = $(testGroupGenerator)
