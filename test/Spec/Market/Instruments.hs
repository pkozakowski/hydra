{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Spec.Market.Instruments where

import Data.Record.Hom
import Market
import Market.Instruments
import Market.Instruments.Test
import Market.Types
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.TH

type ThreeLabels = '["a", "b", "c"]
type Prcs = Prices ThreeLabels
type Port = Portfolio ThreeLabels

test_Hold :: [TestTree]
test_Hold =
    [ testInstrumentLaws @ThreeLabels $ Hold @"a"
    , testProperty "allocation is one point" onePointAllocation
    ] where
        onePointAllocation :: Prcs -> Property
        onePointAllocation prices = alloc === onePoint (labelIn @"a") where
            alloc = initAllocation prices $ Hold @"a"

tests :: TestTree
tests = $(testGroupGenerator)
