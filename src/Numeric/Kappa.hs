{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Numeric.Kappa
    ( Kappa (..)
    , deriveKappaDivision
    , kappaLaws
    , kappaSemimoduleLaws
    , kappaSemimoduleACLaws
    ) where

import Language.Haskell.TH
import Numeric.Algebra
import Numeric.Algebra.Division
import Prelude hiding ((*), (/), pi)
import Test.QuickCheck
import Test.QuickCheck.Classes

-- | Triple of numeric types, where one is a product of two others. Defines
-- the product and the two quotients. Division by zero gives Nothing.
class Kappa a b c | a b -> c, b c -> a, c a -> b where

    -- | Quotient of a and b gives c.
    kappa :: a -> b -> Maybe c

    -- | The other quotient.
    kappa' :: a -> c -> Maybe b

    -- | Product of b and c gives a.
    pi :: b -> c -> a

-- | Derive an instance for three types coercible to a divisible type.
deriveKappaDivision :: Name -> Name -> Name -> Name -> Q [Dec]
deriveKappaDivision scr a b c =
    [d| instance Kappa $(conT a) $(conT b) $(conT c) where
            kappa x y = coerce (coerce x `safeDiv` coerce y :: Maybe $(conT scr))
            kappa' x z = coerce (coerce x `safeDiv` coerce z :: Maybe $(conT scr))
            pi y z = coerce (coerce y * coerce z :: $(conT scr)) |]

safeDiv :: (Division a, Monoidal a, Eq a) => a -> a -> Maybe a
safeDiv x y = if y == zero then Nothing else Just $ x / y

-- | Basic laws, without interaction with other classes.
kappaLaws ::
    ( Kappa a b c
    , Arbitrary a, Arbitrary b, Arbitrary c
    , Eq a, Eq b, Eq c
    , Show a, Show b, Show c
    ) =>
    proxy a -> proxy b -> proxy c -> Laws
kappaLaws ap bp cp = Laws "Kappa"
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

-- | Laws for Kappa + (semi) Module (a, b, c).
kappaSemimoduleLaws ::
    ( Kappa a b c
    , LeftModule scr a, LeftModule scr b, LeftModule scr c
    , Division scr
    , Monoidal scr
    , Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary scr
    , Eq a, Eq b, Eq c, Eq scr
    , Show a, Show b, Show c, Show scr
    ) =>
    proxy a -> proxy b -> proxy c -> proxy scr -> Laws
kappaSemimoduleLaws ap bp cp scrp = Laws "Kappa + (semi) Module (a, b, c)"
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

-- Laws for Kappa + (semi) Module (a, c).
kappaSemimoduleACLaws ::
    ( Kappa a b c
    , LeftModule scr a, LeftModule scr c
    , Division scr
    , Monoidal scr
    , Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary scr
    , Eq a, Eq b, Eq c, Eq scr
    , Show a, Show b, Show c, Show scr
    ) =>
    proxy a -> proxy b -> proxy c -> proxy scr -> Laws
kappaSemimoduleACLaws ap bp cp scrp = Laws "Kappa + (semi) Module (a, c)"
    [ ("Pi Inverses Kappa", kappaPiInversesKappa ap bp cp)
    , ("Pi Inverses Kappa Prime", kappaPiInversesKappa' ap bp cp)
    , ("Kappa Inverses Pi", kappaKappaInversesPi ap bp cp)
    , ("Kappa Prime Inverses Pi", kappaKappa'InversesPi ap bp cp)
    , ("Multiplication Agreement", kappaMultiplicationAgreementAC ap bp cp scrp)
    , ("Division Agreement", kappaDivisionAgreementAC ap bp cp scrp)
    ]

kappaMultiplicationAgreementAC ::
    forall proxy a b c scr.
    ( Kappa a b c
    , LeftModule scr a, LeftModule scr c
    , Arbitrary b, Arbitrary c, Arbitrary scr
    , Eq a
    , Show a, Show b, Show c, Show scr
    ) =>
    proxy a -> proxy b -> proxy c -> proxy scr -> Property
kappaMultiplicationAgreementAC _ _ _ _ = property
    $ \(z :: scr) (y' :: b) (z' :: c)
    -> y' `pi` (z .* z') == z .* y' `pi` z'

kappaDivisionAgreementAC ::
    forall proxy a b c scr.
    ( Kappa a b c
    , LeftModule scr a, LeftModule scr c
    , Division scr
    , Monoidal scr
    , Arbitrary a, Arbitrary b, Arbitrary scr
    , Eq c, Eq scr
    , Show a, Show b, Show c, Show scr
    ) =>
    proxy a -> proxy b -> proxy c -> proxy scr -> Property
kappaDivisionAgreementAC _ _ _ _ = property
      $ \(x :: scr) (y :: scr) (x' :: a) (y' :: b)
     -> y /= zero
    ==> (x .* x') `kappa` y' == fmap (x .*) (x' `kappa` y')
