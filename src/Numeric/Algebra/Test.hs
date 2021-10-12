{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}

module Numeric.Algebra.Test where

import Data.Constraint
import Data.Maybe
import Numeric.Algebra
import Numeric.Domain.GCD
import Numeric.Field.Fraction
import Prelude hiding ((<), (>), (+), (-), (*), negate, subtract)
import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.Tasty

instance Arbitrary Natural where
    arbitrary = arbitrarySizedNatural
    shrink = shrinkIntegral

instance (Arbitrary i, Integral i, GCDDomain i) => Arbitrary (Fraction i) where

    arbitrary = (%) <$> arbitrary <*> (arbitrary `suchThat` isNotZero) where
        isNotZero = (/= 0) . fromIntegral

    shrink frac = ((% den) <$> shrink num) ++ ((num %) <$> shrinkDen) where
        shrinkDen = filter ((/= 0) . fromIntegral) $ shrink den
        num = numerator frac
        den = denominator frac 

additiveLaws
    :: (Additive r, Arbitrary r, Eq r, Show r) => proxy r -> Laws
additiveLaws p = Laws "Additive"
    [ ("Associativity", additiveAssociativity p)
    , ("Unit", additiveUnit p)
    , ("Double", additiveDouble p)
    , ("Double + Unit", additiveDoublePlusUnit p)
    ]

additiveAssociativity
    :: forall r proxy. (Additive r, Arbitrary r, Eq r, Show r)
    => proxy r -> Property
additiveAssociativity _ = property
    $ \(x :: r) (y :: r) (z :: r)
    -> (x + y) + z == x + (y + z)

additiveUnit
    :: forall r proxy. (Additive r, Arbitrary r, Eq r, Show r)
    => proxy r -> Property
additiveUnit _ = property
    $ \(x :: r)
    -> sinnum1p 0 x == x

additiveDouble
    :: forall r proxy. (Additive r, Arbitrary r, Eq r, Show r)
    => proxy r -> Property
additiveDouble _ = property
    $ \(n :: Natural) (x :: r)
    -> sinnum1p (2 * n + 1) x == sinnum1p n x + sinnum1p n x

additiveDoublePlusUnit
    :: forall r proxy. (Additive r, Arbitrary r, Eq r, Show r)
    => proxy r -> Property
additiveDoublePlusUnit _ = property
    $ \(n :: Natural) (x :: r)
    -> sinnum1p (2 * n + 2) x == sinnum1p n x + sinnum1p n x + x

abelianLaws :: (Abelian r, Arbitrary r, Eq r, Show r) => proxy r -> Laws
abelianLaws p = Laws "Abelian"
    [ ("Commutativity", abelianCommutativity p)
    ]

abelianCommutativity
    :: forall r proxy. (Abelian r, Arbitrary r, Eq r, Show r)
    => proxy r -> Property
abelianCommutativity _ = property
    $ \(x :: r) (y :: r)
    -> x + y == y + x

monoidalLaws
    :: (Monoidal r, Arbitrary r, Eq r, Show r) => proxy r -> Laws
monoidalLaws p = Laws "Monoidal"
    [ ("Left Identity", monoidalLeftIdentity p)
    , ("Right Identity", monoidalRightIdentity p)
    , ("Sinnum", monoidalSinnum p)
    ]

monoidalLeftIdentity
    :: forall r proxy. (Monoidal r, Arbitrary r, Eq r, Show r)
    => proxy r -> Property
monoidalLeftIdentity _ = property
    $ \(x :: r)
    -> zero + x == x

monoidalRightIdentity
    :: forall r proxy. (Monoidal r, Arbitrary r, Eq r, Show r)
    => proxy r -> Property
monoidalRightIdentity _ = property
    $ \(x :: r)
    -> x + zero == x

monoidalSinnum
    :: forall r proxy. (Monoidal r, Arbitrary r, Eq r, Show r)
    => proxy r -> Property
monoidalSinnum _ = property
    $ \(n :: Natural) (x :: r)
    -> sinnum1p n x == sinnum (1 + n) x

groupLaws :: (Group r, Arbitrary r, Eq r, Show r) => proxy r -> Laws
groupLaws p = Laws "Group"
    [ ("Left Inverse", groupLeftInverse p)
    , ("Right Inverse", groupRightInverse p)
    , ("Minus", groupMinus p)
    , ("Subtract", groupSubtract p)
    , ("Times Positive", groupTimesPositive p)
    , ("Times Negative", groupTimesNegative p)
    ]

groupLeftInverse
    :: forall r proxy. (Group r, Arbitrary r, Eq r, Show r)
    => proxy r -> Property
groupLeftInverse _ = property
    $ \(x :: r)
    -> negate x + x == zero

groupRightInverse
    :: forall r proxy. (Group r, Arbitrary r, Eq r, Show r)
    => proxy r -> Property
groupRightInverse _ = property
    $ \(x :: r)
    -> x + negate x == zero

groupMinus
    :: forall r proxy. (Group r, Arbitrary r, Eq r, Show r)
    => proxy r -> Property
groupMinus _ = property
    $ \(x :: r) (y :: r)
    -> x - y == x + negate y

groupSubtract
    :: forall r proxy. (Group r, Arbitrary r, Eq r, Show r)
    => proxy r -> Property
groupSubtract _ = property
    $ \(x :: r) (y :: r)
    -> subtract x y == negate x + y

groupTimesPositive
    :: forall r proxy. (Group r, Arbitrary r, Eq r, Show r)
    => proxy r -> Property
groupTimesPositive _ = property
    $ \(n :: Natural) (x :: r)
    -> fromIntegral n `times` x == sinnum n x

groupTimesNegative
    :: forall r proxy. (Group r, Arbitrary r, Eq r, Show r)
    => proxy r -> Property
groupTimesNegative _ = property
    $ \(n :: Natural) (x :: r)
    -> negate (fromIntegral n :: Integer) `times` x == negate (sinnum n x)

type LeftModuleConstraints r m =
    ( Rig r
    , LeftModule r m
    , Arbitrary r
    , Arbitrary m
    , Eq m
    , Show r
    , Show m
    )

leftModuleLaws
    :: LeftModuleConstraints r m
    => proxy r -> proxy m -> Laws
leftModuleLaws rp mp = Laws "LeftModule"
    [ ("Module Distributivity", leftModuleModuleDistributivity rp mp)
    , ("Scalar Distributivity", leftModuleModuleDistributivity rp mp)
    , ("Associativity", leftModuleAssociativity rp mp)
    , ("Identity", leftModuleIdentity rp mp)
    ]

leftModuleMonoidalLaws
    :: (LeftModuleConstraints r m, Monoidal m)
    => proxy r -> proxy m -> Laws
leftModuleMonoidalLaws rp mp = Laws "LeftModule + Monoidal"
    [ ("Module Distributivity", leftModuleModuleDistributivity rp mp)
    , ("Scalar Distributivity", leftModuleModuleDistributivity rp mp)
    , ("Associativity", leftModuleAssociativity rp mp)
    , ("Identity", leftModuleIdentity rp mp)
    , ("Scalar Annihilation", leftModuleScalarAnnihilation rp mp)
    , ("Module Annihilation", leftModuleModuleAnnihilation rp mp)
    ]

leftModuleModuleDistributivity
    :: forall r m proxy
     . LeftModuleConstraints r m
    => proxy r -> proxy m -> Property
leftModuleModuleDistributivity _ _ = property
    $ \(a :: r) (x :: m) (y :: m)
    -> a .* (x + y) == a .* x + a .* y

leftModuleScalarDistributivity
    :: forall r m proxy
     . LeftModuleConstraints r m
    => proxy r -> proxy m -> Property
leftModuleScalarDistributivity _ _ = property
    $ \(a :: r) (b :: r) (x :: m)
    -> (a + b) .* x == a .* x + b .* x

leftModuleAssociativity
    :: forall r m proxy
     . LeftModuleConstraints r m
    => proxy r -> proxy m -> Property
leftModuleAssociativity _ _ = property
    $ \(a :: r) (b :: r) (x :: m)
    -> (a * b) .* x == a .* (b .* x)

leftModuleIdentity
    :: forall r m proxy
     . LeftModuleConstraints r m
    => proxy r -> proxy m -> Property
leftModuleIdentity _ _ = property
    $ \(x :: m)
    -> (one :: r) .* x == x

leftModuleScalarAnnihilation
    :: forall r m proxy
     . (LeftModuleConstraints r m, Monoidal m)
    => proxy r -> proxy m -> Property
leftModuleScalarAnnihilation _ _ = property
    $ \(x :: m)
    -> (zero :: r) .* x == zero

leftModuleModuleAnnihilation
    :: forall r m proxy
     . (LeftModuleConstraints r m, Monoidal m)
    => proxy r -> proxy m -> Property
leftModuleModuleAnnihilation _ _ = property
    $ \(a :: r)
    -> a .* (zero :: m) == zero

type RightModuleConstraints r m =
    ( Rig r
    , RightModule r m
    , Arbitrary r
    , Arbitrary m
    , Eq m
    , Show r
    , Show m
    )

rightModuleLaws
    :: RightModuleConstraints r m
    => proxy r -> proxy m -> Laws
rightModuleLaws rp mp = Laws "RightModule"
    [ ("Module Distributivity", rightModuleModuleDistributivity rp mp)
    , ("Scalar Distributivity", rightModuleModuleDistributivity rp mp)
    , ("Associativity", rightModuleAssociativity rp mp)
    , ("Identity", rightModuleIdentity rp mp)
    ]

rightModuleMonoidalLaws
    :: (RightModuleConstraints r m, Monoidal m)
    => proxy r -> proxy m -> Laws
rightModuleMonoidalLaws rp mp = Laws "RightModule + Monoidal"
    [ ("Module Distributivity", rightModuleModuleDistributivity rp mp)
    , ("Scalar Distributivity", rightModuleModuleDistributivity rp mp)
    , ("Associativity", rightModuleAssociativity rp mp)
    , ("Identity", rightModuleIdentity rp mp)
    , ("Scalar Annihilation", rightModuleScalarAnnihilation rp mp)
    , ("Module Annihilation", rightModuleModuleAnnihilation rp mp)
    ]

rightModuleModuleDistributivity
    :: forall r m proxy
     . RightModuleConstraints r m
    => proxy r -> proxy m -> Property
rightModuleModuleDistributivity _ _ = property
    $ \(x :: m) (y :: m) (a :: r)
    -> (x + y) *. a == x *. a + y *. a

rightModuleScalarDistributivity
    :: forall r m proxy
     . RightModuleConstraints r m
    => proxy r -> proxy m -> Property
rightModuleScalarDistributivity _ _ = property
    $ \(x :: m) (a :: r) (b :: r)
    -> x *. (a + b) == x *. a + x *. b

rightModuleAssociativity
    :: forall r m proxy
     . RightModuleConstraints r m
    => proxy r -> proxy m -> Property
rightModuleAssociativity _ _ = property
    $ \(x :: m) (a :: r) (b :: r)
    -> x *. (a * b) == (x *. a) *. b

rightModuleIdentity
    :: forall r m proxy
     . RightModuleConstraints r m
    => proxy r -> proxy m -> Property
rightModuleIdentity _ _ = property
    $ \(x :: m)
    -> x *. (one :: r) == x

rightModuleScalarAnnihilation
    :: forall r m proxy
     . (RightModuleConstraints r m, Monoidal m)
    => proxy r -> proxy m -> Property
rightModuleScalarAnnihilation _ _ = property
    $ \(x :: m)
    -> x *. (zero :: r) == zero

rightModuleModuleAnnihilation
    :: forall r m proxy
     . (RightModuleConstraints r m, Monoidal m)
    => proxy r -> proxy m -> Property
rightModuleModuleAnnihilation _ _ = property
    $ \(a :: r)
    -> (zero :: m) *. a == zero

allSemimoduleLaws ::
    ( Rig r
    , Abelian m
    , Monoidal m
    , LeftModule r m
    , RightModule r m
    , Arbitrary r
    , Arbitrary m
    , Eq m
    , Show r
    , Show m
    ) =>
    proxy r -> proxy m -> [Laws]
allSemimoduleLaws rp mp =
    [ additiveLaws mp
    , abelianLaws mp
    , monoidalLaws mp
    , leftModuleMonoidalLaws rp mp
    , rightModuleMonoidalLaws rp mp
    ]

allModuleLaws ::
    ( Ring r
    , Abelian m
    , Group m
    , LeftModule r m
    , RightModule r m
    , Arbitrary r
    , Arbitrary m
    , Eq m
    , Show r
    , Show m
    ) =>
    proxy r -> proxy m -> [Laws]
allModuleLaws rp mp
    = groupLaws mp : allSemimoduleLaws rp mp

orderLaws
    :: (Order o, Arbitrary o, Eq o, Show o) => proxy o -> Laws
orderLaws p = Laws "Order"
    [ ("Reflexivity", orderReflexivity p)
    , ("Transitivity", orderTransitivity p)
    , ("Antisymmetry", orderAntisymmetry p)
    , ("Order Agreement", orderOrderAgreement p)
    , ("Comparable Agreement", orderComparableAgreement p)
    ]

orderReflexivity
    :: forall o proxy
     . (Order o, Arbitrary o, Eq o, Show o)
    => proxy o -> Property
orderReflexivity _ = property
    $ \(x :: o)
    -> x ~~ x

orderTransitivity
    :: forall o proxy
     . (Order o, Arbitrary o, Eq o, Show o)
    => proxy o -> Property
orderTransitivity _ = property
    $ \(x :: o) (y :: o) (z :: o)
   -> collect (x `order` y, y `order` z)
    $ not (x <~ y && y <~ z) || x <~ z

orderAntisymmetry
    :: forall o proxy
     . (Order o, Arbitrary o, Eq o, Show o)
    => proxy o -> Property
orderAntisymmetry _ = property
    $ \(x :: o) (y :: o)
   -> collect (x `order` y)
    $ not (x <~ y && y <~ x) || x == y

orderOrderAgreement
    :: forall o proxy
     . (Order o, Arbitrary o, Eq o, Show o)
    => proxy o -> Property
orderOrderAgreement _ = property
    $ \(x :: o) (y :: o)
   -> collect (x `order` y)
    $ case x `order` y of
        Just LT -> x <~ y && x < y && x /~ y && not (x >~ y || x > y || x ~~ y)
        Just EQ -> x <~ y && x >~ y && x ~~ y && not (x < y || x > y || x /~ y)
        Just GT -> x >~ y && x > y && x /~ y && not (x <~ y || x < y || x ~~ y)
        Nothing -> x /~ y && not (x <~ y || x < y || x >~ y || x > y || x ~~ y)

orderComparableAgreement
    :: forall o proxy
     . (Order o, Arbitrary o, Eq o, Show o)
    => proxy o -> Property
orderComparableAgreement _ = property
    $ \(x :: o) (y :: o)
    -> x `comparable` y == isJust (x `order` y)
