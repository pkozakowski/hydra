{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Market.Types
    ( module Market.Asset
    , module Market.Types
    ) where

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.Bifunctor
import Data.Coerce
import Data.Fixed
import Data.Functor.Apply
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Sparse hiding (Value)
import Data.Map.Static hiding (Value)
import Data.Maybe
import Data.Proxy
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable
import Data.String
import Data.Time
import qualified Dhall as Dh
import Dhall (FromDhall)
import GHC.Generics
import GHC.TypeLits
import Market.Asset
import Market.Deriving
import Numeric.Algebra hiding ((<), (>), fromInteger)
import Numeric.Algebra.Test
import Numeric.Delta
import Numeric.Field.Fraction
import Numeric.Kappa
import Numeric.Normed
import Numeric.Truncatable
import Prelude hiding ((+), (-), (*), (/), negate, pi, sum)
import qualified Prelude
import Test.QuickCheck
import Test.QuickCheck.Instances

instance FromDhall NominalDiffTime where

    autoWith _ = Dh.union
        ( ( fromNatural    1 <$> Dh.constructor "Seconds" Dh.natural )
       <> ( fromNatural   60 <$> Dh.constructor "Minutes" Dh.natural )
       <> ( fromNatural 3600 <$> Dh.constructor "Hours"   Dh.natural )
        ) where
            fromNatural mul = (mul Prelude.*) . fromInteger . toInteger

-- | Underlying scalar type.
type Scalar = Fraction Integer

instance LeftModule Scalar Scalar where
    (.*) = (*)

instance RightModule Scalar Scalar where
    (*.) = (*)

instance Module Scalar Scalar

instance Default Scalar where
    def = zero

instance FromDhall Scalar where
    autoWith _ = truncateTo (Proxy @E12) . realToFraction <$> Dh.double

instance NFData Scalar where
    rnf frac
            = rnf (numerator frac)
        `seq` rnf (denominator frac)
        `seq` frac
        `seq` ()

newtype InstrumentName = InstrumentName { unInstrumentName :: String }
    deriving (Eq, Generic, Ord)
    deriving newtype (IsString, NFData, Semigroup, Show)

instance FromDhall InstrumentName where
    autoWith _
        = Dh.record
        $ Dh.field "instrumentName"
        $ InstrumentName <$> Dh.string

-- | Amount of an Asset.
newtype Amount = Amount Scalar
newtype AmountDelta = AmountDelta Scalar

-- | Portfolio is a record of Amounts for each Asset.
newtype Portfolio = Portfolio (SparseMap Asset Amount)
newtype PortfolioDelta = PortfolioDelta (SparseMap Asset AmountDelta)

deriveAdditiveQuantityInstances
    ''Scalar ''Amount ''AmountDelta ''Portfolio ''PortfolioDelta

-- | Price of an Asset, measured in units of some baseline asset (e.g. USD).
newtype Price = Price Scalar
newtype PriceDelta = PriceDelta Scalar

-- | Record of Prices for each Asset.
newtype Prices = Prices (StaticMap Asset Price)
newtype PriceDeltas = PriceDeltas (SparseMap Asset PriceDelta)

deriveNonAdditiveQuantityInstances
    ''Scalar ''Price ''PriceDelta ''Prices ''PriceDeltas

-- | Possessed Value, measured in units of some baseline asset (e.g. USD).
newtype Value = Value Scalar
newtype ValueDelta = ValueDelta Scalar

-- | Value held in each Asset.
newtype Values = Values (SparseMap Asset Value)
newtype ValueDeltas = ValueDeltas (SparseMap Asset ValueDelta)

deriveAdditiveQuantityInstances
    ''Scalar ''Value ''ValueDelta ''Values ''ValueDeltas

-- | Value = Price * Amount.
deriveKappaDivision ''Scalar ''Value ''Price ''Amount
deriveKappaDivision ''Scalar ''ValueDelta ''PriceDelta ''Amount
deriveKappaDivision ''Scalar ''ValueDelta ''Price ''AmountDelta
deriveKappaNewtype ''Values ''Prices ''Portfolio
deriveKappaNewtype ''ValueDeltas ''PriceDeltas ''Portfolio
deriveKappaNewtype ''ValueDeltas ''Prices ''PortfolioDelta

-- | Share of some quantity in a bigger whole.
newtype Share = Share Scalar
newtype ShareDelta = ShareDelta Scalar

-- | Safe constructor for Share.
share :: Scalar -> Share
share x = assert (x >= zero && x <= one) $ Share x

-- | Amount = Amount * Share.
deriveKappaDivision ''Scalar ''Amount ''Amount ''Share
deriveKappaDivision ''Scalar ''AmountDelta ''AmountDelta ''Share
deriveKappaDivision ''Scalar ''AmountDelta ''Amount ''ShareDelta

-- | Value = Value * Share.
deriveKappaDivision ''Scalar ''Value ''Value ''Share
deriveKappaDivision ''Scalar ''ValueDelta ''ValueDelta ''Share
deriveKappaDivision ''Scalar ''ValueDelta ''Value ''ShareDelta

-- | Distribution on Assets. Sums to 1.
newtype Distribution = Distribution (SparseMap Asset Share)

-- | Delta between two Distributions. Sums to 0.
newtype DistributionDelta = DistributionDelta (SparseMap Asset ShareDelta)

deriveConstrainedQuantityInstances
    ''Scalar ''Share ''ShareDelta ''Distribution ''DistributionDelta

-- | Only Values are Normed, because it only makes sense to add up Values held
-- in different Assets.
deriveScalable ''Scalar ''Distribution ''Value ''Values
deriveScalable ''Scalar ''DistributionDelta ''ValueDelta ''ValueDeltas
deriveNormed ''Scalar ''Distribution ''Value ''Values

instance Truncatable Distribution where

    truncateTo res (Distribution shares)
        = Distribution $ share <$> xs'' where
            xs'' = addToFirst (one - sum xs') xs' where
                addToFirst
                    :: Scalar
                    -> SparseMap Asset Scalar
                    -> SparseMap Asset Scalar
                addToFirst toAdd m = case toList m of
                    [] -> m
                    (k, v) : _ -> insert k (v + toAdd) m
            xs' = truncateTo res xs
            xs = unShare <$> shares where
                unShare (Share x) = x

onePoint :: Asset -> Distribution
onePoint asset = Distribution $ fromList [(asset, share one)]

distributionValid :: Distribution -> Bool
distributionValid (Distribution shares)
    =  all (>= zero) shares'
    && foldl (+) zero shares' == one where
        shares' = unShare <$> shares where
            unShare (Share scalar) = scalar

distributionDeltaValid :: DistributionDelta -> Bool
distributionDeltaValid (DistributionDelta shareDeltas)
    = foldl (+) zero shareDeltas' == zero where
        shareDeltas' = unShareDelta <$> shareDeltas where
            unShareDelta (ShareDelta scalar) = scalar

type SomeAmount = (Asset, Amount)

transfer :: SomeAmount -> PortfolioDelta
transfer (asset, amount)
    = PortfolioDelta $ fromList [(asset, amount `delta` zero)]

data Fees = Fees
    { variable :: Scalar
    , fixed :: Maybe SomeAmount
    } deriving (Eq, Generic, NFData, Show)

instance Order Fees where

    fs <~ fs' = leqFixed && variable fs <= variable fs' where
        leqFixed = case (fixed fs, fixed fs') of
            (Just (asset, amount), Just (asset', amount'))
                -> asset == asset' && amount <= amount'
            (Nothing, _)
                -> True
            (Just (_, amount), Nothing)
                -> False

zeroFees :: Fees
zeroFees = Fees { fixed = Nothing, variable = 0 % 1 }

feeAssets :: Fees -> [Asset]
feeAssets fees = case fixed fees of
    Just (asset, _) -> [asset]
    Nothing -> []

data OrderAmount
    = Absolute Amount
    | Relative Share
    deriving Show

orderAmountIsZero :: OrderAmount -> Bool
orderAmountIsZero = \case
    Absolute amount -> amount == zero
    Relative share -> share == Share zero

everything :: OrderAmount
everything = Relative $ share one

type TimeStep a = (UTCTime, a)

newtype TimeSeries a = TimeSeries { unTimeSeries :: NonEmpty (TimeStep a) }
    deriving (Functor, Foldable, Show, Traversable)
    deriving newtype (Semigroup)

deriving anyclass instance Foldable1 TimeSeries

instance Traversable1 TimeSeries where
    sequence1 = fmap TimeSeries . sequence1 . fmap engulf . unTimeSeries where
        engulf (t, ax) = (t,) <$> ax

instance Apply TimeSeries where
    fs <.> xs = uncurry ($) <$> zipSeries fs xs

seriesFromList :: [TimeStep a] -> Maybe (TimeSeries a)
seriesFromList = fmap TimeSeries . nonEmpty

seriesToList :: TimeSeries a -> [TimeStep a]
seriesToList = NonEmpty.toList . unTimeSeries

zipSeries :: TimeSeries a -> TimeSeries b -> TimeSeries (a, b)
zipSeries s1 s2
    = fromJust
    $ seriesFromList
    $ go Nothing Nothing (seriesToList s1, seriesToList s2) where
        go prev1 prev2 = \case
            ((t1, x1) : txs1, (t2, x2) : txs2)
                | t1 > t2 -> case prev1 of
                    Just x1' -> (t2, (x1', x2)) : advance1
                    Nothing -> advance1
                | t1 < t2 -> case prev2 of
                    Just x2' -> (t1, (x1, x2')) : advance2
                    Nothing -> advance2
                | t1 == t2 -> (t1, (x1, x2)) : advance12
                where 
                    advance1 = go prev1 (Just x2) ((t1, x1) : txs1, txs2)
                    advance2 = go (Just x1) prev2 (txs1, (t2, x2) : txs2)
                    advance12 = go (Just x1) (Just x2) (txs1, txs2)
            ([], txs2) -> rest (,) prev1 txs2
            (txs1, []) -> rest (flip (,)) prev2 txs1
            where
                rest f prev txs = case prev of
                    Just x  -> second (f x) <$> txs
                    Nothing -> []

-- Arbitrary instances and other test utilities:

instance Arbitrary Amount where
    arbitrary = Amount <$> arbitraryNonNegativeScalar
    shrink (Amount scr) = Amount <$> shrinkNonNegativeScalar scr

deriving newtype instance Arbitrary AmountDelta
deriving newtype instance Arbitrary Portfolio
deriving newtype instance Arbitrary PortfolioDelta

instance Arbitrary Price where
    arbitrary = Price <$> arbitraryPositiveScalar
    shrink (Price scr) = Price <$> shrinkPositiveScalar scr

deriving newtype instance Arbitrary PriceDelta

instance Arbitrary Prices where
    arbitrary = Prices . fromList . zip testAssets <$> infiniteList
    shrink (Prices prices) = Prices <$> hasAllTestAssets `filter` shrink prices

instance Arbitrary PriceDeltas where
    arbitrary = PriceDeltas . fromList . zip testAssets <$> infiniteList
    shrink (PriceDeltas prices)
        = PriceDeltas <$> hasAllTestAssets `filter` shrink prices

hasAllTestAssets :: (ReadMap Asset v m) => m -> Bool
hasAllTestAssets map = sort (fst <$> toList map) == testAssets

instance Arbitrary Value where
    arbitrary = Value <$> arbitraryNonNegativeScalar
    shrink (Value scr) = Value <$> shrinkNonNegativeScalar scr

deriving newtype instance Arbitrary ValueDelta
deriving newtype instance Arbitrary Values
deriving newtype instance Arbitrary ValueDeltas

instance Arbitrary Share where

    arbitrary
        = Share <$> arbitraryNonNegativeScalar `suchThat` \x -> x <= one

    shrink (Share scr)
        = Share <$> filter (<= one) (shrinkPositiveScalar scr)

deriving newtype instance Arbitrary ShareDelta

instance Arbitrary Distribution where

    arbitrary = Distribution . normalize <$> ensureNonzero arbitraryPosMap where
        normalize r = Share <$> (/ sum r) <$> r
        ensureNonzero gen = gen `suchThat` \dist -> sum dist /= zero
        sum = foldl (+) zero
        arbitraryPosMap :: Gen (SparseMap Asset Scalar)
        arbitraryPosMap = coerce <$> (arbitrary :: Gen (SparseMap Asset Share))

    shrink _ = []

instance Arbitrary DistributionDelta where
    arbitrary = delta <$> arbitrary <*> arbitrary
    shrink _ = []

arbitraryNonNegativeScalar :: Gen Scalar
arbitraryNonNegativeScalar
      = (%)
    <$> arbitrary `suchThat` isNonNegative
    <*> arbitrary `suchThat` isPositive where
        isNonNegative = (>= 0) . fromIntegral
        isPositive = (> 0) . fromIntegral

shrinkNonNegativeScalar :: Scalar -> [Scalar]
shrinkNonNegativeScalar scr
    = ((% den) <$> shrinkNum) ++ ((num %) <$> shrinkDen) where
        shrinkNum = filter ((>= 0) . fromIntegral) $ shrink num
        shrinkDen = filter ((> 0) . fromIntegral) $ shrink den
        num = numerator scr
        den = denominator scr 

arbitraryPositiveScalar :: Gen Scalar
arbitraryPositiveScalar = arbitraryNonNegativeScalar `suchThat` (/= zero)

shrinkPositiveScalar :: Scalar -> [Scalar]
shrinkPositiveScalar scr = filter (/= zero) $ shrinkNonNegativeScalar scr

instance Arbitrary OrderAmount where

    arbitrary = either Absolute Relative <$> arbitrary

    shrink = \case
        Absolute amount -> Absolute <$> shrink amount
        Relative share  -> Relative <$> shrink share

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

instance Arbitrary Fees where

    arbitrary = Fees
        <$> arbitrary `suchThat` validShare
        <*> fmap decrease arbitrary `suchThat` positiveIfJust where
            decrease
                = fmap \(asset, amount)
               -> (asset, ((1 % 100) :: Fraction Integer) .* amount)

    shrink fees = Fees
        <$> validShare `filter` (shrink $ variable fees)
        <*> positiveIfJust `filter` shrink (fixed fees)

positiveIfJust :: Maybe SomeAmount -> Bool
positiveIfJust = maybe True $ (> zero) . snd

validShare :: Scalar -> Bool
validShare x = x >= zero && x <= one
