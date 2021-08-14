module Numeric.Algebra.Test where

import Numeric.Algebra
import Numeric.Domain.GCD
import Numeric.Field.Fraction
import Prelude hiding ((+), (-), (*), negate, subtract)
import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.Tasty
import Test.Tasty.QuickCheck.Laws

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

testAdditiveLaws
    :: (Additive r, Arbitrary r, Eq r, Show r) => proxy r -> TestTree
testAdditiveLaws p = testLaws $ Laws "Additive"
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

testAbelianLaws :: (Abelian r, Arbitrary r, Eq r, Show r) => proxy r -> TestTree
testAbelianLaws p = testLaws $ Laws "Abelian"
    [ ("Commutativity", abelianCommutativity p)
    ]

abelianCommutativity
    :: forall r proxy. (Abelian r, Arbitrary r, Eq r, Show r)
    => proxy r -> Property
abelianCommutativity _ = property
    $ \(x :: r) (y :: r)
    -> x + y == y + x

testMonoidalLaws
    :: (Monoidal r, Arbitrary r, Eq r, Show r) => proxy r -> TestTree
testMonoidalLaws p = testLaws $ Laws "Monoidal"
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

testGroupLaws :: (Group r, Arbitrary r, Eq r, Show r) => proxy r -> TestTree
testGroupLaws p = testLaws $ Laws "Group"
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

testLeftModuleLaws
    ::  ( Rig r
        , Monoidal m
        , LeftModule r m
        , Arbitrary r
        , Arbitrary m
        , Eq m
        , Show r
        , Show m
        )
    => proxy r -> proxy m -> TestTree
testLeftModuleLaws rp mp = testLaws $ Laws "LeftModule"
    [ ("Module Distributivity", leftModuleModuleDistributivity rp mp)
    , ("Scalar Distributivity", leftModuleModuleDistributivity rp mp)
    , ("Associativity", leftModuleAssociativity rp mp)
    , ("Identity", leftModuleIdentity rp mp)
    , ("Scalar Annihilation", leftModuleScalarAnnihilation rp mp)
    , ("Module Annihilation", leftModuleModuleAnnihilation rp mp)
    ]

type ModuleConstraints r m proxy =
     ( Rig r
     , Monoidal m
     , LeftModule r m
     , Arbitrary r
     , Arbitrary m
     , Eq m
     , Show r
     , Show m
     )

leftModuleModuleDistributivity
    :: forall r m proxy
     . ModuleConstraints r m proxy
    => proxy r -> proxy m -> Property
leftModuleModuleDistributivity _ _ = property
    $ \(a :: r) (x :: m) (y :: m)
    -> a .* (x + y) == a .* x + a .* y

leftModuleScalarDistributivity
    :: forall r m proxy
     . ModuleConstraints r m proxy
    => proxy r -> proxy m -> Property
leftModuleScalarDistributivity _ _ = property
    $ \(a :: r) (b :: r) (x :: m)
    -> (a + b) .* x == a .* x + b .* x

leftModuleAssociativity
    :: forall r m proxy
     . ModuleConstraints r m proxy
    => proxy r -> proxy m -> Property
leftModuleAssociativity _ _ = property
    $ \(a :: r) (b :: r) (x :: m)
    -> (a * b) .* x == a .* (b .* x)

leftModuleIdentity
    :: forall r m proxy
     . ModuleConstraints r m proxy
    => proxy r -> proxy m -> Property
leftModuleIdentity _ _ = property
    $ \(x :: m)
    -> (one :: r) .* x == x

leftModuleScalarAnnihilation
    :: forall r m proxy
     . ModuleConstraints r m proxy
    => proxy r -> proxy m -> Property
leftModuleScalarAnnihilation _ _ = property
    $ \(x :: m)
    -> (zero :: r) .* x == zero

leftModuleModuleAnnihilation
    :: forall r m proxy
     . ModuleConstraints r m proxy
    => proxy r -> proxy m -> Property
leftModuleModuleAnnihilation _ _ = property
    $ \(a :: r)
    -> a .* (zero :: m) == zero

testRightModuleLaws
    :: ModuleConstraints r m proxy
    => proxy r -> proxy m -> TestTree
testRightModuleLaws rp mp = testLaws $ Laws "RightModule"
    [ ("Module Distributivity", rightModuleModuleDistributivity rp mp)
    , ("Scalar Distributivity", rightModuleModuleDistributivity rp mp)
    , ("Associativity", rightModuleAssociativity rp mp)
    , ("Identity", rightModuleIdentity rp mp)
    , ("Scalar Annihilation", rightModuleScalarAnnihilation rp mp)
    , ("Module Annihilation", rightModuleModuleAnnihilation rp mp)
    ]

rightModuleModuleDistributivity
    :: forall r m proxy
     . ModuleConstraints r m proxy
    => proxy r -> proxy m -> Property
rightModuleModuleDistributivity _ _ = property
    $ \(x :: m) (y :: m) (a :: r)
    -> (x + y) *. a == x *. a + y *. a

rightModuleScalarDistributivity
    :: forall r m proxy
     . ModuleConstraints r m proxy
    => proxy r -> proxy m -> Property
rightModuleScalarDistributivity _ _ = property
    $ \(x :: m) (a :: r) (b :: r)
    -> x *. (a + b) == x *. a + x *. b

rightModuleAssociativity
    :: forall r m proxy
     . ModuleConstraints r m proxy
    => proxy r -> proxy m -> Property
rightModuleAssociativity _ _ = property
    $ \(x :: m) (a :: r) (b :: r)
    -> x *. (a * b) == (x *. a) *. b

rightModuleIdentity
    :: forall r m proxy
     . ModuleConstraints r m proxy
    => proxy r -> proxy m -> Property
rightModuleIdentity _ _ = property
    $ \(x :: m)
    -> x *. (one :: r) == x

rightModuleScalarAnnihilation
    :: forall r m proxy
     . ModuleConstraints r m proxy
    => proxy r -> proxy m -> Property
rightModuleScalarAnnihilation _ _ = property
    $ \(x :: m)
    -> x *. (zero :: r) == zero

rightModuleModuleAnnihilation
    :: forall r m proxy
     . ModuleConstraints r m proxy
    => proxy r -> proxy m -> Property
rightModuleModuleAnnihilation _ _ = property
    $ \(a :: r)
    -> (zero :: m) *. a == zero

testAllSemimoduleLaws ::
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
    proxy r -> proxy m -> [TestTree]
testAllSemimoduleLaws rp mp =
    [ testAdditiveLaws mp
    , testAbelianLaws mp
    , testMonoidalLaws mp
    , testLeftModuleLaws rp mp
    , testRightModuleLaws rp mp
    ]

testAllModuleLaws ::
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
    proxy r -> proxy m -> [TestTree]
testAllModuleLaws rp mp
    = testAllSemimoduleLaws rp mp ++ [testGroupLaws mp]
