{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.Market.Types where

import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Static hiding (Value)
import Data.Proxy
import Data.Typeable
import Market.Ops
import Market.Types
import Numeric.Algebra hiding ((-))
import Numeric.Algebra.Test
import Numeric.Delta
import Numeric.Kappa
import Numeric.Normed
import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.QuickCheck.Laws
import Test.Tasty.TH

showProxyType :: Typeable (Proxy t) => Proxy t -> String
showProxyType = show . head . typeRepArgs . typeOf

testAllAdditiveQuantityLaws ::
    ( NonAdditiveQuantityConstraints scr q qd qm qdm
    , Abelian q, Abelian qm
    , Monoidal q, Monoidal qm
    , LeftModule scr q, LeftModule scr qm
    , RightModule scr q, RightModule scr qm
    ) =>
    Proxy scr -> Proxy q -> Proxy qd -> Proxy qm -> Proxy qdm -> [TestTree]
testAllAdditiveQuantityLaws pscr pq pqd pqm pqdm =
    [ testGroup nq
        $ (testLaws <$> allSemimoduleLaws pscr pq)
    , testGroup nqm
        $ (testLaws <$> allSemimoduleLaws pscr pqm)
    , testGroup (nq ++ " + " ++ nqd)
        $ [testLaws $ deltaMonoidalOrdLaws pq pqd]
    , testGroup (nqm ++ " + " ++ nqdm)
        $ [testLaws $ deltaMonoidalLaws pqm pqdm]
    ] ++ testAllNonAdditiveQuantityLaws pscr pq pqd pqm pqdm where
        nq = showProxyType pq
        nqd = showProxyType pqd
        nqm = showProxyType pqm
        nqdm = showProxyType pqdm

type NonAdditiveQuantityConstraints scr q qd qm qdm =
    ( Ring scr
    , Abelian qd, Abelian qdm
    , Group qd, Group qdm
    , LeftModule scr qd, LeftModule scr qdm
    , RightModule scr qd, RightModule scr qdm
    , Delta q qd, Delta qm qdm
    , Arbitrary scr, Arbitrary q, Arbitrary qd, Arbitrary qm, Arbitrary qdm
    , Eq q, Eq qd, Eq qm, Eq qdm
    , Ord q, Ord qd
    , Show scr, Show q, Show qd, Show qm, Show qdm
    , Typeable q, Typeable qd, Typeable qm, Typeable qdm
    )

testAllNonAdditiveQuantityLaws
    :: NonAdditiveQuantityConstraints scr q qd qm qdm
    => Proxy scr -> Proxy q -> Proxy qd -> Proxy qm -> Proxy qdm -> [TestTree]
testAllNonAdditiveQuantityLaws pscr pq pqd pqm pqdm =
    [ testGroup nq
        $ testCommonScalarLaws pq
    , testGroup nqd
        $ (testLaws <$> allModuleLaws pscr pqd)
       ++ testCommonScalarLaws pqd
    , testGroup nqm
        $ testCommonMapLaws pqm
    , testGroup (nq ++ " + " ++ nqd)
        $ [testLaws $ deltaLaws pq pqd]
    , testGroup nqdm
        $ (testLaws <$> allModuleLaws pscr pqdm)
       ++ testCommonMapLaws pqdm
    , testGroup (nqm ++ " + " ++ nqdm)
        $ [testLaws $ deltaLaws pqm pqdm]
    ] where
        nq = showProxyType pq
        nqd = showProxyType pqd
        nqm = showProxyType pqm
        nqdm = showProxyType pqdm

testCommonScalarLaws
    :: (Eq a, Ord a, Show a, Arbitrary a)
    => Proxy a -> [TestTree]
testCommonScalarLaws p =
    [ testLaws (eqLaws p)
    , testLaws (ordLaws p)
    , testLaws (showLaws p)
    ]

testCommonMapLaws :: (Eq a, Show a, Arbitrary a) => Proxy a -> [TestTree]
testCommonMapLaws p =
    [ testLaws (eqLaws p)
    , testLaws (showLaws p)
    ]

test_quantity_laws_for_Portfolio :: [TestTree]
test_quantity_laws_for_Portfolio = testAllAdditiveQuantityLaws
    @Scalar @Amount @AmountDelta @Portfolio @PortfolioDelta
    p p p p p

test_quantity_laws_for_Prices :: [TestTree]
test_quantity_laws_for_Prices = testAllNonAdditiveQuantityLaws
    @Scalar @Price @PriceDelta @Prices @PriceDeltas
    p p p p p

test_quantity_laws_for_Values :: [TestTree]
test_quantity_laws_for_Values = testAllAdditiveQuantityLaws
    @Scalar @Value @ValueDelta @Values @ValueDeltas
    p p p p p

test_Kappa_Value_Amount_Price :: [TestTree]
test_Kappa_Value_Amount_Price
    = [testLaws $ kappaLaws @Value @Price @Amount p p p]

test_Kappa_ValueDelta_AmountDelta_Price :: [TestTree]
test_Kappa_ValueDelta_AmountDelta_Price
    = [testLaws $ kappaLaws @ValueDelta @Price @AmountDelta p p p]

test_Kappa_ValueDelta_Amount_PriceDelta :: [TestTree]
test_Kappa_ValueDelta_Amount_PriceDelta
    = [testLaws $ kappaLaws @ValueDelta @PriceDelta @Amount p p p]

test_Kappa_Values_Portfolio_Prices :: [TestTree]
test_Kappa_Values_Portfolio_Prices =
    [ testLaws $ kappaSemimoduleACLaws
        @Values @Prices @Portfolio @Scalar
        p p p p
    ]

test_Kappa_ValueDeltas_Portfolio_PriceDeltas :: [TestTree]
test_Kappa_ValueDeltas_Portfolio_PriceDeltas =
    [ testLaws $ kappaSemimoduleACLaws
        @ValueDeltas @PriceDeltas @Portfolio @Scalar
        p p p p
    ]

test_Kappa_ValueDeltas_PortfolioDelta_Prices :: [TestTree]
test_Kappa_ValueDeltas_PortfolioDelta_Prices =
    [ testLaws $ kappaSemimoduleACLaws
        @ValueDeltas @PriceDeltas @Portfolio @Scalar
        p p p p
    ]

testAllDistributionLaws ::
    forall scr q qd qm qdm.
    ( Ring scr
    , Abelian qd, Abelian qdm
    , Group qd, Group qdm
    , LeftModule scr qd, LeftModule scr qdm
    , RightModule scr qd, RightModule scr qdm
    , Delta q qd, Delta qm qdm
    , Arbitrary scr, Arbitrary q, Arbitrary qd, Arbitrary qm, Arbitrary qdm
    , Eq q, Eq qd, Eq qm, Eq qdm
    , Ord q, Ord qd
    , Show scr, Show q, Show qd, Show qm, Show qdm
    , Typeable q, Typeable qd, Typeable qm, Typeable qdm
    ) =>
    Proxy scr -> Proxy q -> Proxy qd -> Proxy qm -> Proxy qdm -> [TestTree]
testAllDistributionLaws pscr pq pqd pqm pqdm =
    [ testGroup nq
         $ testCommonScalarLaws pq
    , testGroup nqd
         $ (testLaws <$> allModuleLaws pscr pqd)
        ++ testCommonScalarLaws pqd
    , testGroup (nq ++ " + " ++ nqd)
         $ [testLaws $ deltaLaws pq pqd]
    , testGroup nqm
         $ testCommonMapLaws pqm
    , testGroup nqdm
         $ (testLaws <$> allModuleLaws pscr pqdm)
        ++ testCommonMapLaws pqdm
    , testGroup (nqm ++ " + " ++ nqdm)
         $ [testLaws $ deltaLaws pqm pqdm]
    ] where
        nq = showProxyType pq
        nqd = showProxyType pqd
        nqm = showProxyType pqm
        nqdm = showProxyType pqdm

test_distribution_laws_for_Distribution :: [TestTree]
test_distribution_laws_for_Distribution = testAllDistributionLaws
    @Scalar @Share @ShareDelta
    @(Distribution Asset) @(DistributionDelta Asset)
    p p p p p

test_Normalizable_Distribution_Value_Values :: [TestTree]
test_Normalizable_Distribution_Value_Values =
    [ testLaws $ normedSemimoduleOrdLaws
        @(Distribution Asset) @Value @Values @Scalar p p p p
    ]

test_Fees :: [TestTree]
test_Fees = [testLaws $ orderLaws @Fees p]

test_zipSeries :: [TestTree]
test_zipSeries =
    [ testProperty "agreement with sweep" agreementWithSweep
    ] where
        agreementWithSweep :: TimeSeries Int -> TimeSeries Int -> Property
        agreementWithSweep xs ys
            = counterexample ("zipped: " ++ show zipped)
            $ counterexample ("events: " ++ show events)
            $ length zipped >= minLength .&&. diff >= 0 .&&. conjoin
                (zipWith agree (seriesToList zipped) (drop diff events))
            where
                zipped = zipSeries xs ys
                events = sweep $ fromList
                    [(False, seriesToList xs), (True, seriesToList ys)]
                minLength = min (length xs) (length ys)
                diff = length events - length zipped
                agree (t, (x, y)) (t', Event changes)
                    = t === t' .&&. conjoin (NonEmpty.toList $ ok <$> changes)
                        where
                            ok (k, v) = case k of
                                False -> v === x
                                True  -> v === y

p :: Proxy a
p = Proxy

tests :: TestTree
tests = $(testGroupGenerator)
