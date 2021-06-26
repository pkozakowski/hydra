{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Spec.Market.Types where

import Data.Proxy
import Data.Record.Hom.Test
import Data.Typeable
import Market.Types
import Numeric.Algebra hiding ((>), (>=))
import Numeric.Algebra.Test
import Numeric.Delta
import Numeric.Domain.GCD
import Numeric.Field.Fraction
import Numeric.Test
import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.Tasty
import Test.Tasty.QuickCheck.Laws
import Test.Tasty.TH

type ThreeLabels = '["a", "b", "c"]

instance Arbitrary Amount where
    arbitrary = Amount <$> arbitraryPositiveFraction
    shrink (Amount frac) = Amount <$> shrinkPositiveFraction frac

deriving instance Arbitrary AmountDelta
deriving instance Arbitrary (Portfolio ThreeLabels)
deriving instance Arbitrary (PortfolioDelta ThreeLabels)

instance Arbitrary Price where
    arbitrary = Price <$> arbitraryPositiveFraction
    shrink (Price frac) = Price <$> shrinkPositiveFraction frac

deriving instance Arbitrary PriceDelta
deriving instance Arbitrary (Prices ThreeLabels)
deriving instance Arbitrary (PriceDeltas ThreeLabels)

instance Arbitrary Value where
    arbitrary = Value <$> arbitraryPositiveFraction
    shrink (Value frac) = Value <$> shrinkPositiveFraction frac

deriving instance Arbitrary ValueDelta
deriving instance Arbitrary (Values ThreeLabels)
deriving instance Arbitrary (ValueDeltas ThreeLabels)

arbitraryPositiveFraction :: (Arbitrary i, Integral i, GCDDomain i) => Gen (Fraction i)
arbitraryPositiveFraction
    = (%) <$> arbitrary `suchThat` isNonNegative <*> arbitrary `suchThat` isPositive where
        isNonNegative = (>= 0) . fromIntegral
        isPositive = (> 0) . fromIntegral

shrinkPositiveFraction :: (Arbitrary i, Integral i, GCDDomain i) => Fraction i -> [Fraction i]
shrinkPositiveFraction frac = ((% den) <$> shrinkNum) ++ ((num %) <$> shrinkDen) where
    shrinkNum = filter ((>= 0) . fromIntegral) $ shrink num
    shrinkDen = filter ((> 0) . fromIntegral) $ shrink den
    num = numerator frac
    den = denominator frac 

showProxyType :: Typeable t => Proxy t -> String
showProxyType = show . head . typeRepArgs . typeOf

testAllQuantityLaws ::
    ( Ring scr
    , Abelian q, Abelian qd, Abelian qr, Abelian qdr
    , Monoidal q, Monoidal qr
    , Group qd, Group qdr
    , LeftModule scr q, LeftModule scr qd, LeftModule scr qr, LeftModule scr qdr
    , RightModule scr q, RightModule scr qd, RightModule scr qr, RightModule scr qdr
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
        testCommonScalarLaws p =
            [ testLaws (eqLaws p)
            , testLaws (ordLaws p)
            , testLaws (showLaws p)
            ]
        testCommonRecordLaws p =
            [ testLaws (eqLaws p)
            , testLaws (showLaws p)
            ]
        nq = showProxyType pq
        nqd = showProxyType pqd
        nqr = showProxyType pqr
        nqdr = showProxyType pqdr

test_quantity_laws_for_Portfolio :: [TestTree]
test_quantity_laws_for_Portfolio = testAllQuantityLaws
    @Scalar @Amount @AmountDelta @(Portfolio ThreeLabels) @(PortfolioDelta ThreeLabels) p p p p p

test_quantity_laws_for_Prices :: [TestTree]
test_quantity_laws_for_Prices = testAllQuantityLaws
    @Scalar @Price @PriceDelta @(Prices ThreeLabels) @(PriceDeltas ThreeLabels) p p p p p

test_quantity_laws_for_Values :: [TestTree]
test_quantity_laws_for_Values = testAllQuantityLaws
    @Scalar @Value @ValueDelta @(Values ThreeLabels) @(ValueDeltas ThreeLabels) p p p p p

test_Kappa_Value_Amount_Price :: [TestTree]
test_Kappa_Value_Amount_Price
    = [testKappaLaws @Value @Amount @Price p p p]

test_Kappa_deltas_of_Value_Amount_Price :: [TestTree]
test_Kappa_deltas_of_Value_Amount_Price
    = [testKappaLaws @ValueDelta @AmountDelta @PriceDelta p p p]

test_Kappa_Values_Portfolio_Prices :: [TestTree]
test_Kappa_Values_Portfolio_Prices =
    [ testKappaLaws
        @(Values ThreeLabels)
        @(Portfolio ThreeLabels)
        @(Prices ThreeLabels)
        p p p
    ]

test_Kappa_deltas_of_Values_Portfolio_Prices :: [TestTree]
test_Kappa_deltas_of_Values_Portfolio_Prices =
    [ testKappaLaws
        @(ValueDeltas ThreeLabels)
        @(PortfolioDelta ThreeLabels)
        @(PriceDeltas ThreeLabels)
        p p p
    ]

p :: Proxy a
p = Proxy

-- TODO: Distribution and (Un)Normalizable laws

tests :: TestTree
tests = $(testGroupGenerator)
