{-# LANGUAGE ScopedTypeVariables #-}

module Numeric.Test where

import Data.Maybe
import Numeric.Algebra
import Numeric.Delta
import Numeric.Kappa
import Prelude hiding ((+), (-), (*), (/), pi, recip)
import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.Tasty
import Test.Tasty.QuickCheck.Laws

testDeltaLaws
    :: (Delta a b, Arbitrary a, Eq a, Eq b, Show a)
    => proxy a -> proxy b -> TestTree
testDeltaLaws ap bp = testLaws $ Laws "Delta"
    [ ("Sigma Inverses Delta", deltaSigmaInversesDelta ap bp)
    ]

deltaSigmaInversesDelta
    :: forall a b proxy
    .  (Delta a b, Arbitrary a, Eq a, Show a)
    => proxy a -> proxy b -> Property
deltaSigmaInversesDelta _ _ = property
    $ \(x :: a) (y :: a)
    -> x `sigma` (y `delta` x) == Just y

testDeltaMonoidalLaws
    :: (Delta a b, Monoidal a, Monoidal b, Arbitrary a, Eq a, Eq b, Show a)
    => proxy a -> proxy b -> TestTree
testDeltaMonoidalLaws ap bp = testLaws $ Laws "Delta + Monoidal"
    [ ("Sigma Inverses Delta", deltaSigmaInversesDelta ap bp)
    , ("Right Identity", deltaRightIdentity ap bp)
    , ("Addition Agreement", deltaAdditionAgreement ap bp)
    , ("Subtraction Agreement", deltaSubtractionAgreement ap bp)
    ]

deltaRightIdentity
    :: forall a b proxy. (Delta a b, Monoidal b, Arbitrary a, Eq a, Show a)
    => proxy a -> proxy b -> Property
deltaRightIdentity _ _ = property
    $ \(x :: a)
    -> x `sigma` zero == Just x

deltaAdditionAgreement
    :: forall a b proxy. (Delta a b, Monoidal a, Monoidal b, Arbitrary a, Eq a, Show a)
    => proxy a -> proxy b -> Property
deltaAdditionAgreement _ _ = property test where
    test (x :: a) (y :: a)
        = x `sigma` yrel == zero `sigma` (xrel + yrel) where
            xrel = x `delta` zero
            yrel = y `delta` zero

deltaSubtractionAgreement
    :: forall a b proxy. (Delta a b, Monoidal a, Arbitrary a, Eq b, Show a)
    => proxy a -> proxy b -> Property
deltaSubtractionAgreement _ _ = property test where
    test (x :: a) (y :: a)
        = x `delta` y == xrel - yrel where
            xrel = x `delta` zero
            yrel = y `delta` zero

testDeltaMonoidalOrdLaws ::
    ( Delta a b
    , Monoidal a
    , Monoidal b
    , Ord b
    , Arbitrary a
    , Arbitrary b
    , Eq a
    , Eq b
    , Show a
    , Show b
    ) =>
    proxy a -> proxy b -> TestTree
testDeltaMonoidalOrdLaws ap bp = testLaws $ Laws "Delta + Monoidal + Ord"
    [ ("Sigma Inverses Delta", deltaSigmaInversesDelta ap bp)
    , ("Right Identity", deltaRightIdentity ap bp)
    , ("Addition Agreement", deltaAdditionAgreement ap bp)
    , ("Subtraction Agreement", deltaSubtractionAgreement ap bp)
    , ("Positive Totality", deltaPositiveTotality ap bp)
    ]

deltaPositiveTotality
    :: forall a b proxy. (Delta a b, Monoidal a, Ord b, Arbitrary b, Eq a, Show b)
    => proxy a -> proxy b -> Property
deltaPositiveTotality _ _ = property
    $ \(x :: b)
    -> isJust (zero `sigma` x) == (x >= zero)

testKappaLaws ::
    ( Kappa a b c
    , Arbitrary a, Arbitrary b, Arbitrary c
    , Eq a, Eq b, Eq c
    , Show a, Show b, Show c
    ) =>
    proxy a -> proxy b -> proxy c -> TestTree
testKappaLaws ap bp cp = testLaws $ Laws "Kappa"
    [ ("Pi Inverses Kappa", kappaPiInversesKappa ap bp cp)
    , ("Pi Inverses Kappa Prime", kappaPiInversesKappa' ap bp cp)
    , ("Kappa Inverses Pi", kappaKappaInversesPi ap bp cp)
    , ("Kappa Prime Inverses Pi", kappaKappa'InversesPi ap bp cp)
    ]

kappaPiInversesKappa ::
    forall proxy a b c.
    ( Kappa a b c
    , Arbitrary a, Arbitrary b
    , Eq a
    , Show a, Show b, Show c
    ) =>
    proxy a -> proxy b -> proxy c -> Property
kappaPiInversesKappa _ _ _ = property
    $ \(x :: a) (y :: b)
    -> fmap (y `pi`) (x `kappa` y) `elem` [Just x, Nothing]

kappaPiInversesKappa' ::
    forall proxy a b c.
    ( Kappa a b c
    , Arbitrary a, Arbitrary c
    , Eq a
    , Show a, Show b, Show c
    ) =>
    proxy a -> proxy b -> proxy c -> Property
kappaPiInversesKappa' _ _ _ = property
    $ \(x :: a) (z :: c)
    -> fmap (`pi` z) (x `kappa'` z) `elem` [Just x, Nothing]

kappaKappaInversesPi ::
    forall proxy a b c.
    ( Kappa a b c
    , Arbitrary b, Arbitrary c
    , Eq c
    , Show a, Show b, Show c
    ) =>
    proxy a -> proxy b -> proxy c -> Property
kappaKappaInversesPi _ _ _ = property
    $ \(y :: b) (z :: c)
    -> (y `pi` z) `kappa` y `elem` [Just z, Nothing]

kappaKappa'InversesPi ::
    forall proxy a b c.
    ( Kappa a b c
    , Arbitrary b, Arbitrary c
    , Eq b
    , Show a, Show b, Show c
    ) =>
    proxy a -> proxy b -> proxy c -> Property
kappaKappa'InversesPi _ _ _ = property
    $ \(y :: b) (z :: c)
    -> (y `pi` z) `kappa'` z `elem` [Just y, Nothing]
