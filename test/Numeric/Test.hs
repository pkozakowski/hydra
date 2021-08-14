module Numeric.Test where

import Data.Maybe
import Data.Proxy
import Numeric.Algebra
import Numeric.Delta
import Numeric.Kappa
import Numeric.Normalizable
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
    :: forall a b proxy
     . (Delta a b, Monoidal a, Monoidal b, Arbitrary a, Eq a, Show a)
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
    , ("Positive Definiteness", deltaPositiveDefiniteness ap bp)
    ]

deltaPositiveDefiniteness
    :: forall a b proxy
     . (Delta a b, Monoidal a, Ord b, Arbitrary b, Eq a, Show b)
    => proxy a -> proxy b -> Property
deltaPositiveDefiniteness _ _ = property
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

testKappaSemimoduleLaws ::
    ( Kappa a b c
    , LeftModule scr a, LeftModule scr b, LeftModule scr c
    , Division scr
    , Monoidal scr
    , Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary scr
    , Eq a, Eq b, Eq c, Eq scr
    , Show a, Show b, Show c, Show scr
    ) =>
    proxy a -> proxy b -> proxy c -> proxy scr -> TestTree
testKappaSemimoduleLaws ap bp cp scrp = testLaws $ Laws "Kappa (semi) Module"
    [ ("Pi Inverses Kappa", kappaPiInversesKappa ap bp cp)
    , ("Pi Inverses Kappa Prime", kappaPiInversesKappa' ap bp cp)
    , ("Kappa Inverses Pi", kappaKappaInversesPi ap bp cp)
    , ("Kappa Prime Inverses Pi", kappaKappa'InversesPi ap bp cp)
    , ("Multiplication Agreement", kappaMultiplicationAgreement ap bp cp scrp)
    , ("Division Agreement", kappaDivisionAgreement ap bp cp scrp)
    , ("Division Agreement Prime", kappaDivisionAgreementPrime ap bp cp scrp)
    ]

kappaMultiplicationAgreement ::
    forall proxy a b c scr.
    ( Kappa a b c
    , LeftModule scr a, LeftModule scr b, LeftModule scr c
    , Arbitrary b, Arbitrary c, Arbitrary scr
    , Eq a
    , Show a, Show b, Show c, Show scr
    ) =>
    proxy a -> proxy b -> proxy c -> proxy scr -> Property
kappaMultiplicationAgreement _ _ _ _ = property
    $ \(y :: scr) (z :: scr) (y' :: b) (z' :: c)
    -> (y .* y') `pi` (z .* z') == y * z .* y' `pi` z'

kappaDivisionAgreement ::
    forall proxy a b c scr.
    ( Kappa a b c
    , LeftModule scr a, LeftModule scr b, LeftModule scr c
    , Division scr
    , Monoidal scr
    , Arbitrary a, Arbitrary b, Arbitrary scr
    , Eq c, Eq scr
    , Show a, Show b, Show c, Show scr
    ) =>
    proxy a -> proxy b -> proxy c -> proxy scr -> Property
kappaDivisionAgreement _ _ _ _ = property
      $ \(x :: scr) (y :: scr) (x' :: a) (y' :: b)
     -> y /= zero
    ==> (x .* x') `kappa` (y .* y') == fmap (x / y .*) (x' `kappa` y')

kappaDivisionAgreementPrime ::
    forall proxy a b c scr.
    ( Kappa a b c
    , LeftModule scr a, LeftModule scr b, LeftModule scr c
    , Division scr
    , Monoidal scr
    , Arbitrary a, Arbitrary c, Arbitrary scr
    , Eq b, Eq scr
    , Show a, Show b, Show c, Show scr
    ) =>
    proxy a -> proxy b -> proxy c -> proxy scr -> Property
kappaDivisionAgreementPrime _ _ _ _ = property
      $ \(x :: scr) (z :: scr) (x' :: a) (z' :: c)
     -> z /= zero
    ==> (x .* x') `kappa'` (z .* z') == fmap (x / z .*) (x' `kappa'` z')

testNormalizableLaws ::
    ( Normalizable d a as
    , Arbitrary d, Arbitrary a, Arbitrary as
    , Eq d, Eq a, Eq as
    , Show d, Show a, Show as
    ) =>
    proxy d -> proxy a -> proxy as -> TestTree
testNormalizableLaws dp ap asp = testLaws $ Laws "Normalizable"
    [   ( "Normalize Inverses Unnormalize"
        , normalizableNormalizeInversesUnnormalize dp ap asp
        )
    ,   ("Unnormalize Inverses Normalize"
        , normalizableUnnormalizeInversesNormalize dp ap asp
        )
    ]

normalizableNormalizeInversesUnnormalize 
    :: forall proxy d a as
    .  (Normalizable d a as, Arbitrary a, Arbitrary d, Eq d, Show a, Show d)
    => proxy d -> proxy a -> proxy as -> Property
normalizableNormalizeInversesUnnormalize _ _ _ = property
    $ \(n :: a) (dist :: d)
    -> normalize (unnormalize n dist) `elem` [Just dist, Nothing]

normalizableUnnormalizeInversesNormalize
    :: forall proxy d a as
    .  (Normalizable d a as, Arbitrary as, Eq as, Show as)
    => proxy d -> proxy a -> proxy as -> Property
normalizableUnnormalizeInversesNormalize _ _ _ = property
    $ \(xs :: as)
    -> case normalize xs of
        Just dist -> unnormalize (norm xs) dist == xs
        Nothing   -> True

testNormalizableSemimoduleOrdLaws ::
    forall d a as scr.
    ( Normalizable d a as
    , LeftModule scr a, LeftModule scr as
    , Monoidal a, Monoidal as, Monoidal scr
    , Ord scr, Ord a
    , Arbitrary d, Arbitrary a, Arbitrary as, Arbitrary scr
    , Eq d, Eq a, Eq as
    , Show d, Show a, Show as, Show scr
    ) =>
    Proxy d -> Proxy a -> Proxy as -> Proxy scr -> TestTree
testNormalizableSemimoduleOrdLaws dp ap asp scrp
    = testLaws $ Laws "Normalizable + (semi) Module + Ord"
        [   ( "Normalize Inverses Unnormalize"
            , normalizableNormalizeInversesUnnormalize dp ap asp
            )
        ,   ( "Unnormalize Inverses Normalize"
            , normalizableUnnormalizeInversesNormalize dp ap asp
            )
        ,   ( "Triangle Inequality"
            , normalizableTriangleInequality dp ap asp scrp)
        ,   ( "Absolute Homogeneity"
            , normalizableAbsoluteHomogeneity dp ap asp scrp
            )
        ,   ( "Norm Positive Definiteness"
            , normalizableNormPositiveDefiniteness dp ap asp scrp
            )
        ,   ( "Normalize Positive Definiteness"
            , normalizableNormalizePositiveDefiniteness dp ap asp scrp
            )
        ]

normalizableTriangleInequality ::
    forall proxy d a as scr.
    ( Normalizable d a as
    , Additive a, Additive as
    , Ord a
    , Arbitrary as
    , Eq as
    , Show as
    ) =>
    proxy d -> proxy a -> proxy as -> proxy scr -> Property
normalizableTriangleInequality _ _ _ _ = property
    $ \(xs :: as) (ys :: as)
    -> norm (xs + ys) <= norm xs + norm ys

normalizableAbsoluteHomogeneity ::
    forall proxy d a as scr.
    ( Normalizable d a as
    , LeftModule scr a, LeftModule scr as
    , Monoidal scr
    , Ord scr
    , Arbitrary as, Arbitrary scr
    , Eq a
    , Show as, Show scr
    ) =>
    proxy d -> proxy a -> proxy as -> proxy scr -> Property
normalizableAbsoluteHomogeneity _ _ _ _ = property
    $ \(x :: scr) (ys :: as)
    -> x >= zero ==> norm (x .* ys) == x .* norm ys

normalizableNormPositiveDefiniteness ::
    forall proxy d a as scr.
    ( Normalizable d a as
    , Monoidal a, Monoidal as
    , Arbitrary as
    , Eq a, Eq as
    , Show as
    ) =>
    proxy d -> proxy a -> proxy as -> proxy scr -> Property
normalizableNormPositiveDefiniteness _ _ _ _ = property
    $ \(xs :: as)
    -> norm xs /= zero || xs == zero

normalizableNormalizePositiveDefiniteness ::
    forall proxy d a as scr.
    ( Normalizable d a as
    , Monoidal a, Monoidal as
    , Arbitrary as
    , Eq a, Eq as
    , Show as
    ) =>
    proxy d -> proxy a -> proxy as -> proxy scr -> Property
normalizableNormalizePositiveDefiniteness _ _ _ _ = property
    $ \(xs :: as)
    -> isJust (normalize xs) || xs == zero
