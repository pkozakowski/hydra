{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Numeric.Delta
    ( Delta (..)
    , deltaLaws
    , deltaMonoidalLaws
    , deltaMonoidalOrdLaws
    , deriveDeltaOrd
    ) where

import Data.Coerce
import Data.Maybe
import Language.Haskell.TH
import Numeric.Algebra
import Prelude hiding ((+), (-))
import Test.QuickCheck
import Test.QuickCheck.Classes

-- | Pairing between two types, where the second is a Group and represents
-- the difference of two values of the first type. In some cases it can be
-- integrated back into a value of the first type.
class Group b => Delta a b | a -> b, b -> a where

    -- | Difference between two a gives b.
    delta :: a -> a -> b

    -- | Opportunity to reintegrate b into a. May fail.
    sigma :: a -> b -> Maybe a

-- | Derive an instance for a comparable type with the same representation as
-- another type.
deriveDeltaOrd :: Name -> Name -> Q [Dec]
deriveDeltaOrd abs rel =
    [d| instance Delta $(conT abs) $(conT rel) where
            delta x y = coerce x - coerce y
            sigma x y = if z >= zero then Just (coerce z) else Nothing
                where z = coerce x + y |]

-- | Basic laws, without interaction with other classes.
deltaLaws
    :: (Delta a b, Arbitrary a, Eq a, Eq b, Show a, Show b)
    => proxy a -> proxy b -> Laws
deltaLaws ap bp = Laws "Delta"
    [ ("Sigma Inverses Delta", deltaSigmaInversesDelta ap bp)
    ]

deltaSigmaInversesDelta
    :: forall a b proxy
    .  (Delta a b, Arbitrary a, Eq a, Show a, Show b)
    => proxy a -> proxy b -> Property
deltaSigmaInversesDelta _ _ = property
    $ \(x :: a) (y :: a)
    -> counterexample ("result: " ++ show (x `sigma` (y `delta` x))) $
       counterexample ("delta: " ++ show (y `delta` x)) $
        x `sigma` (y `delta` x) == Just y

-- | Laws for Delta + Monoidal.
deltaMonoidalLaws
    ::  ( Delta a b
        , Monoidal a
        , Monoidal b
        , Arbitrary a
        , Eq a
        , Eq b
        , Show a
        , Show b
        )
    => proxy a -> proxy b -> Laws
deltaMonoidalLaws ap bp = Laws "Delta + Monoidal"
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

-- | Laws for Delta + Monoidal + Ord.
deltaMonoidalOrdLaws ::
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
    proxy a -> proxy b -> Laws
deltaMonoidalOrdLaws ap bp = Laws "Delta + Monoidal + Ord"
    [ ("Sigma Inverses Delta", deltaSigmaInversesDelta ap bp)
    , ("Right Identity", deltaRightIdentity ap bp)
    , ("Addition Agreement", deltaAdditionAgreement ap bp)
    , ("Subtraction Agreement", deltaSubtractionAgreement ap bp)
    , ("Positive Semidefiniteness", deltaPositiveSemidefiniteness ap bp)
    ]

deltaPositiveSemidefiniteness
    :: forall a b proxy
     . (Delta a b, Monoidal a, Ord b, Arbitrary b, Eq a, Show b)
    => proxy a -> proxy b -> Property
deltaPositiveSemidefiniteness _ _ = property
    $ \(x :: b)
    -> isJust (zero `sigma` x) == (x >= zero)
