{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.Market.Types where

import Data.Proxy
import Data.Typeable
import Market.Types
import Market.Types.Test
import Numeric.Algebra
import Numeric.Algebra.Test
import Numeric.Delta
import Numeric.Test
import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.Tasty
import Test.Tasty.QuickCheck.Laws
import Test.Tasty.TH

type ThreeLabels = '["a", "b", "c"]

showProxyType :: Typeable (Proxy t) => Proxy t -> String
showProxyType = show . head . typeRepArgs . typeOf

testAllQuantityLaws ::
    ( Ring scr
    , Abelian q, Abelian qd, Abelian qr, Abelian qdr
    , Monoidal q, Monoidal qr
    , Group qd, Group qdr
    , LeftModule scr q, LeftModule scr qd
    , LeftModule scr qr, LeftModule scr qdr
    , RightModule scr q, RightModule scr qd
    , RightModule scr qr, RightModule scr qdr
    , Delta q qd, Delta qr qdr
    , Arbitrary scr, Arbitrary q, Arbitrary qd, Arbitrary qr, Arbitrary qdr
    , Eq q, Eq qd, Eq qr, Eq qdr
    , Ord q, Ord qd
    , Show scr, Show q, Show qd, Show qr, Show qdr
    , Typeable q, Typeable qd, Typeable qr, Typeable qdr
    ) =>
    Proxy scr -> Proxy q -> Proxy qd -> Proxy qr -> Proxy qdr -> [TestTree]
testAllQuantityLaws pscr pq pqd pqr pqdr =
    [ testGroup nq
        $  testAllSemimoduleLaws pscr pq
        ++ testCommonScalarLaws pq
    , testGroup nqd
        $  testAllModuleLaws pscr pqd
        ++ testCommonScalarLaws pqd
    , testGroup (nq ++ " + " ++ nqd)
        $  [testDeltaMonoidalOrdLaws pq pqd]
    , testGroup nqr
        $  testAllSemimoduleLaws pscr pqr
        ++ testCommonRecordLaws pqr
    , testGroup nqdr
        $  testAllModuleLaws pscr pqdr
        ++ testCommonRecordLaws pqdr
    , testGroup (nqr ++ " + " ++ nqdr)
        $  [testDeltaMonoidalLaws pqr pqdr]
    ] where
        nq = showProxyType pq
        nqd = showProxyType pqd
        nqr = showProxyType pqr
        nqdr = showProxyType pqdr

testCommonScalarLaws
    :: (Eq a, Ord a, Show a, Arbitrary a)
    => Proxy a -> [TestTree]
testCommonScalarLaws p =
    [ testLaws (eqLaws p)
    , testLaws (ordLaws p)
    , testLaws (showLaws p)
    ]

testCommonRecordLaws :: (Eq a, Show a, Arbitrary a) => Proxy a -> [TestTree]
testCommonRecordLaws p =
    [ testLaws (eqLaws p)
    , testLaws (showLaws p)
    ]

test_quantity_laws_for_Portfolio :: [TestTree]
test_quantity_laws_for_Portfolio = testAllQuantityLaws
    @Scalar @Amount @AmountDelta
    @(Portfolio ThreeLabels) @(PortfolioDelta ThreeLabels)
    p p p p p

test_quantity_laws_for_Prices :: [TestTree]
test_quantity_laws_for_Prices = testAllQuantityLaws
    @Scalar @Price @PriceDelta @(Prices ThreeLabels) @(PriceDeltas ThreeLabels)
    p p p p p

test_quantity_laws_for_Values :: [TestTree]
test_quantity_laws_for_Values = testAllQuantityLaws
    @Scalar @Value @ValueDelta @(Values ThreeLabels) @(ValueDeltas ThreeLabels)
    p p p p p

test_Kappa_Value_Amount_Price :: [TestTree]
test_Kappa_Value_Amount_Price
    = [testKappaLaws @Value @Amount @Price p p p]

test_Kappa_ValueDelta_AmountDelta_Price :: [TestTree]
test_Kappa_ValueDelta_AmountDelta_Price
    = [testKappaLaws @ValueDelta @AmountDelta @Price p p p]

test_Kappa_ValueDelta_Amount_PriceDelta :: [TestTree]
test_Kappa_ValueDelta_Amount_PriceDelta
    = [testKappaLaws @ValueDelta @Amount @PriceDelta p p p]

test_Kappa_Values_Portfolio_Prices :: [TestTree]
test_Kappa_Values_Portfolio_Prices =
    [ testKappaSemimoduleLaws
        @(Values ThreeLabels)
        @(Portfolio ThreeLabels)
        @(Prices ThreeLabels)
        @Scalar
        p p p p
    ]

test_Kappa_ValueDeltas_Portfolio_PriceDeltas :: [TestTree]
test_Kappa_ValueDeltas_Portfolio_PriceDeltas =
    [ testKappaSemimoduleLaws
        @(ValueDeltas ThreeLabels)
        @(Portfolio ThreeLabels)
        @(PriceDeltas ThreeLabels)
        @Scalar
        p p p p
    ]

test_Kappa_ValueDeltas_PortfolioDelta_Prices :: [TestTree]
test_Kappa_ValueDeltas_PortfolioDelta_Prices =
    [ testKappaSemimoduleLaws
        @(ValueDeltas ThreeLabels)
        @(Portfolio ThreeLabels)
        @(PriceDeltas ThreeLabels)
        @Scalar
        p p p p
    ]

testAllDistributionLaws ::
    forall scr q qd qr qdr.
    ( Ring scr
    , Abelian qd, Abelian qdr
    , Group qd, Group qdr
    , LeftModule scr qd, LeftModule scr qdr
    , RightModule scr qd, RightModule scr qdr
    , Delta q qd, Delta qr qdr
    , Arbitrary scr, Arbitrary q, Arbitrary qd, Arbitrary qr, Arbitrary qdr
    , Eq q, Eq qd, Eq qr, Eq qdr
    , Ord q, Ord qd
    , Show scr, Show q, Show qd, Show qr, Show qdr
    , Typeable q, Typeable qd, Typeable qr, Typeable qdr
    ) =>
    Proxy scr -> Proxy q -> Proxy qd -> Proxy qr -> Proxy qdr -> [TestTree]
testAllDistributionLaws pscr pq pqd pqr pqdr =
    [ testGroup nq
        $  testCommonScalarLaws pq
    , testGroup nqd
        $  testAllModuleLaws pscr pqd
        ++ testCommonScalarLaws pqd
    , testGroup (nq ++ " + " ++ nqd)
        $  [testDeltaLaws pq pqd]
    , testGroup nqr
        $  testCommonRecordLaws pqr
    , testGroup nqdr
        $  testAllModuleLaws pscr pqdr
        ++ testCommonRecordLaws pqdr
    , testGroup (nqr ++ " + " ++ nqdr)
        $  [testDeltaLaws pqr pqdr]
    ] where
        nq = showProxyType pq
        nqd = showProxyType pqd
        nqr = showProxyType pqr
        nqdr = showProxyType pqdr

test_distribution_laws_for_Distribution :: [TestTree]
test_distribution_laws_for_Distribution = testAllDistributionLaws
    @Scalar @Share @ShareDelta
    @(Distribution ThreeLabels) @(DistributionDelta ThreeLabels)
    p p p p p

test_Normalizable_Distribution_Value_Values :: [TestTree]
test_Normalizable_Distribution_Value_Values =
    [ testNormalizableSemimoduleOrdLaws
        @(Distribution ThreeLabels) @Value @(Values ThreeLabels) @Scalar p p p p
    ]

p :: Proxy a
p = Proxy

tests :: TestTree
tests = $(testGroupGenerator)
