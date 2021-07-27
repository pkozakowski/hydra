{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Market.Types.Test where

import Data.Coerce
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Record.Hom
import Data.Record.Hom.Test
import Data.Time
import Data.Traversable
import Market.Types
import Numeric.Algebra hiding ((>), (>=))
import Numeric.Algebra.Test
import Numeric.Delta
import Numeric.Domain.GCD
import Numeric.Field.Fraction
import Prelude hiding ((+), (/))
import Test.QuickCheck
import Test.QuickCheck.Instances.Time

instance Arbitrary Amount where
    arbitrary = Amount <$> arbitraryNonNegativeFraction
    shrink (Amount frac) = Amount <$> shrinkNonNegativeFraction frac

deriving instance Arbitrary AmountDelta
deriving instance Labels assets => Arbitrary (Portfolio assets)
deriving instance Labels assets => Arbitrary (PortfolioDelta assets)

instance Arbitrary Price where
    arbitrary = Price <$> arbitraryPositiveFraction
    shrink (Price frac) = Price <$> shrinkPositiveFraction frac

deriving instance Arbitrary PriceDelta
deriving instance Labels assets => Arbitrary (Prices assets)
deriving instance Labels assets => Arbitrary (PriceDeltas assets)

instance Arbitrary Value where
    arbitrary = Value <$> arbitraryNonNegativeFraction
    shrink (Value frac) = Value <$> shrinkNonNegativeFraction frac

deriving instance Arbitrary ValueDelta
deriving instance Labels assets => Arbitrary (Values assets)
deriving instance Labels assets => Arbitrary (ValueDeltas assets)

instance Arbitrary Share where

    arbitrary
        = Share <$> arbitraryNonNegativeFraction `suchThat` \x -> x <= one

    shrink (Share frac) = Share <$> filter (<= one) (shrinkPositiveFraction frac)

deriving instance Arbitrary ShareDelta

instance Labels assets => Arbitrary (Distribution assets) where

    arbitrary = Distribution . normalize <$> ensureNonzero arbitraryPosRec where
        normalize r = Share <$> (/ sum r) <$> r
        ensureNonzero gen = gen `suchThat` \dist -> sum dist /= zero
        sum = foldl (+) zero
        arbitraryPosRec :: Labels assets => Gen (HomRec assets Scalar)
        arbitraryPosRec = coerce <$> (arbitrary :: Gen (HomRec assets Share))

    shrink _ = []

instance Labels assets => Arbitrary (DistributionDelta assets) where
    arbitrary = delta <$> arbitrary <*> arbitrary
    shrink _ = []

arbitraryNonNegativeFraction :: (Arbitrary i, Integral i, GCDDomain i) => Gen (Fraction i)
arbitraryNonNegativeFraction
    = (%) <$> arbitrary `suchThat` isNonNegative <*> arbitrary `suchThat` isPositive where
        isNonNegative = (>= 0) . fromIntegral
        isPositive = (> 0) . fromIntegral

shrinkNonNegativeFraction :: (Arbitrary i, Integral i, GCDDomain i) => Fraction i -> [Fraction i]
shrinkNonNegativeFraction frac = ((% den) <$> shrinkNum) ++ ((num %) <$> shrinkDen) where
    shrinkNum = filter ((>= 0) . fromIntegral) $ shrink num
    shrinkDen = filter ((> 0) . fromIntegral) $ shrink den
    num = numerator frac
    den = denominator frac 

arbitraryPositiveFraction :: (Arbitrary i, Integral i, GCDDomain i) => Gen (Fraction i)
arbitraryPositiveFraction = arbitraryNonNegativeFraction `suchThat` (/= zero)

shrinkPositiveFraction :: (Arbitrary i, Integral i, GCDDomain i) => Fraction i -> [Fraction i]
shrinkPositiveFraction frac = filter (/= zero) $ shrinkNonNegativeFraction frac

instance Arbitrary OrderAmount where

    arbitrary = either Absolute Relative <$> arbitrary

    shrink = \case
        Absolute amount -> Absolute <$> shrink amount
        Relative share  -> Relative <$> shrink share

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = (:|) <$> arbitrary <*> arbitrary
    shrink (x :| xs) = uncurry (:|) <$> shrink (x, xs)

instance Arbitrary a => Arbitrary (TimeSeries a) where

    arbitrary = do
        initTime <- arbitrary
        shape <- arbitrary @[()]
        diffs <- forM shape $ const $ arbitrary `suchThat` (> 0)
        let times = NonEmpty.fromList $ scanl (flip addUTCTime) initTime diffs
        values <- (:|) <$> arbitrary <*> mapM (const arbitrary) shape
        return $ TimeSeries $ NonEmpty.zip times values

    shrink (TimeSeries srs)
        = TimeSeries <$> NonEmpty.zip times <$> shrink values where
            (times, values) = NonEmpty.unzip srs
