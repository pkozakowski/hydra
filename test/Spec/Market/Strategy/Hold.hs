{-# LANGUAGE TemplateHaskell #-}

module Spec.Market.Strategy.Hold where

import Market
import Market.Strategy.Hold
import Market.Strategy.Test
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.QuickCheck.Laws
import Test.Tasty.TH

test_Hold :: [TestTree]
test_Hold =
  [ testLaws $ strategyLaws arbitraryConfig arbitraryConfig
  , testProperty "allocation is one point" onePointAllocation
  ]
  where
    onePointAllocation :: Prices -> Property
    onePointAllocation prices = alloc === onePoint "A"
      where
        alloc = runInit prices (Hold "A") initAllocation

    arbitraryConfig = return $ Hold "A"

tests :: TestTree
tests = $(testGroupGenerator)
