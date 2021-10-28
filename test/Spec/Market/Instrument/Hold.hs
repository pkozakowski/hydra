{-# LANGUAGE TemplateHaskell #-}

module Spec.Market.Instrument.Hold where

import Market
import Market.Instrument.Hold
import Market.Instrument.Test
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.QuickCheck.Laws
import Test.Tasty.TH

test_Hold :: [TestTree]
test_Hold =
    [ testLaws $ instrumentLaws arbitraryConfig arbitraryConfig
    , testProperty "allocation is one point" onePointAllocation
    ] where
        onePointAllocation :: Prices -> Property
        onePointAllocation prices = alloc === onePoint "A" where
            alloc = runInit prices (Hold "A") initAllocation

        arbitraryConfig = return $ Hold "A"

tests :: TestTree
tests = $(testGroupGenerator)
