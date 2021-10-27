{-# LANGUAGE FunctionalDependencies #-}

module Numeric.Normed
    ( Normed (..)
    , Scalable (..)
    , normedLaws
    , normedSemimoduleOrdLaws
    ) where

import Data.Maybe
import Numeric.Algebra
import Test.QuickCheck hiding (scale)
import Test.QuickCheck.Classes
import Prelude hiding ((+))

-- | Normed vector space.
class Scalable d a as => Normed d a as | d a -> as, as -> a d where

    -- | Compute the norm of a vector.
    norm :: as -> a

    -- | Normalize a vector to a unitless distribution. Can fail when the norm
    -- is zero.
    normalize :: as -> Maybe d

-- | Scalable vectors.
class Scalable d a as | d a -> as, as -> a, as -> d where

    -- | Scale a distribution by a norm to get a vector.
    scale :: a -> d -> as

-- | Basic laws, without interaction with other classes.
normedLaws ::
    ( Normed d a as
    , Arbitrary d, Arbitrary a, Arbitrary as
    , Eq d, Eq a, Eq as
    , Show d, Show a, Show as
    ) =>
    proxy d -> proxy a -> proxy as -> Laws
normedLaws dp ap asp = Laws "Normed"
    [   ( "Normalize Inverses Scale"
        , normedNormalizeInversesScale dp ap asp )
    ,   ("Scale Inverses Normalize"
        , normedScaleInversesNormalize dp ap asp )
    ]

normedNormalizeInversesScale 
    :: forall proxy d a as
    .  (Normed d a as, Arbitrary a, Arbitrary d, Eq d, Show a, Show d)
    => proxy d -> proxy a -> proxy as -> Property
normedNormalizeInversesScale _ _ _ = property
    $ \(n :: a) (dist :: d)
    -> normalize (scale n dist) `elem` [Just dist, Nothing]

normedScaleInversesNormalize
    :: forall proxy d a as
    .  (Normed d a as, Arbitrary as, Eq as, Show as)
    => proxy d -> proxy a -> proxy as -> Property
normedScaleInversesNormalize _ _ _ = property
    $ \(xs :: as)
    -> case normalize xs of
        Just dist -> scale (norm xs) dist == xs
        Nothing   -> True

-- | Laws for Normed + (semi) Module + Ord.
normedSemimoduleOrdLaws ::
    forall d a as scr proxy.
    ( Normed d a as
    , LeftModule scr a, LeftModule scr as
    , Monoidal a, Monoidal as, Monoidal scr
    , Ord scr, Ord a
    , Arbitrary d, Arbitrary a, Arbitrary as, Arbitrary scr
    , Eq d, Eq a, Eq as
    , Show d, Show a, Show as, Show scr
    ) =>
    proxy d -> proxy a -> proxy as -> proxy scr -> Laws
normedSemimoduleOrdLaws dp ap asp scrp
    = Laws "Normed + (semi) Module + Ord"
        [   ( "Normalize Inverses Scale"
            , normedNormalizeInversesScale dp ap asp )
        ,   ( "Scale Inverses Normalize"
            , normedScaleInversesNormalize dp ap asp )
        ,   ( "Triangle Inequality"
            , normedTriangleInequality dp ap asp scrp)
        ,   ( "Absolute Homogeneity"
            , normedAbsoluteHomogeneity dp ap asp scrp )
        ,   ( "Norm Positive Definiteness"
            , normedNormPositiveDefiniteness dp ap asp scrp )
        ,   ( "Normalize Positive Definiteness"
            , normedNormalizePositiveDefiniteness dp ap asp scrp )
        ]

normedTriangleInequality ::
    forall proxy d a as scr.
    ( Normed d a as
    , Additive a, Additive as
    , Ord a
    , Arbitrary as
    , Eq as
    , Show as
    ) =>
    proxy d -> proxy a -> proxy as -> proxy scr -> Property
normedTriangleInequality _ _ _ _ = property
    $ \(xs :: as) (ys :: as)
    -> norm (xs + ys) <= norm xs + norm ys

normedAbsoluteHomogeneity ::
    forall proxy d a as scr.
    ( Normed d a as
    , LeftModule scr a, LeftModule scr as
    , Monoidal scr
    , Ord scr
    , Arbitrary as, Arbitrary scr
    , Eq a
    , Show as, Show scr
    ) =>
    proxy d -> proxy a -> proxy as -> proxy scr -> Property
normedAbsoluteHomogeneity _ _ _ _ = property
    $ \(x :: scr) (ys :: as)
    -> x >= zero ==> norm (x .* ys) == x .* norm ys

normedNormPositiveDefiniteness ::
    forall proxy d a as scr.
    ( Normed d a as
    , Monoidal a, Monoidal as
    , Arbitrary as
    , Eq a, Eq as
    , Show as
    ) =>
    proxy d -> proxy a -> proxy as -> proxy scr -> Property
normedNormPositiveDefiniteness _ _ _ _ = property
    $ \(xs :: as)
    -> norm xs /= zero || xs == zero

normedNormalizePositiveDefiniteness ::
    forall proxy d a as scr.
    ( Normed d a as
    , Monoidal a, Monoidal as
    , Arbitrary as
    , Eq a, Eq as
    , Show as
    ) =>
    proxy d -> proxy a -> proxy as -> proxy scr -> Property
normedNormalizePositiveDefiniteness _ _ _ _ = property
    $ \(xs :: as)
    -> isJust (normalize xs) || xs == zero
