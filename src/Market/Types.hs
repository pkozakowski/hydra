{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Market.Types where

import Control.Exception
import Data.Coerce
import Data.List.NonEmpty
import Data.Proxy
import Data.Record.Hom (HomRec(..), Labels, (:=) (..))
import qualified Data.Record.Hom as HR
import Data.Time
import GHC.TypeLits
import Market.Deriving
import Numeric.Algebra
import Numeric.Delta
import Numeric.Field.Fraction
import Numeric.Kappa
import Numeric.Normalizable
import Numeric.Truncatable
import Prelude hiding ((+), (-), (*), (/), pi, sum)

type Asset (asset :: Symbol) = Proxy asset

instance (Semiring a, Additive a) => LeftModule a a where
    (.*) = (*)

instance (Semiring a, Additive a) => RightModule a a where
    (*.) = (*)

instance (Semiring a, Additive a) => Module a a

-- | Underlying scalar type.
type Scalar = Fraction Integer

-- | Amount of a given Asset.
newtype Amount = Amount Scalar
newtype AmountDelta = AmountDelta Scalar

-- | Portfolio is a record of Amounts for each Asset.
newtype Portfolio assets = Portfolio (HomRec assets Amount)
newtype PortfolioDelta assets = PortfolioDelta (HomRec assets AmountDelta)

deriveQuantityInstances
    ''Scalar ''Amount ''AmountDelta ''Portfolio ''PortfolioDelta

-- | Price of a given Asset, measured in units of the baseline asset (e.g. USD).
newtype Price = Price Scalar
newtype PriceDelta = PriceDelta Scalar

-- | Record of Prices for each Asset.
newtype Prices assets = Prices (HomRec assets Price)
newtype PriceDeltas assets = PriceDeltas (HomRec assets PriceDelta)

deriveQuantityInstances ''Scalar ''Price ''PriceDelta ''Prices ''PriceDeltas

-- | Possessed Value, measured in units of the baseline asset (e.g. USD).
newtype Value = Value Scalar
newtype ValueDelta = ValueDelta Scalar

-- | Value held in each Asset.
newtype Values assets = Values (HomRec assets Value)
newtype ValueDeltas assets = ValueDeltas (HomRec assets ValueDelta)

deriveQuantityInstances ''Scalar ''Value ''ValueDelta ''Values ''ValueDeltas

-- | Value = Amount * Price.
deriveKappaDivision ''Scalar ''Value ''Amount ''Price
deriveKappaDivision ''Scalar ''ValueDelta ''AmountDelta ''Price
deriveKappaDivision ''Scalar ''ValueDelta ''Amount ''PriceDelta
HR.deriveKappa ''Values ''Portfolio ''Prices
HR.deriveKappa ''ValueDeltas ''PortfolioDelta ''Prices
HR.deriveKappa ''ValueDeltas ''Portfolio ''PriceDeltas

-- | Share of some quantity in a bigger whole.
newtype Share = Share Scalar
newtype ShareDelta = ShareDelta Scalar

-- | Safe constructor for Share.
share :: Scalar -> Share
share x = assert (x >= zero && x <= one) $ Share x

-- | Distribution on Assets. Sums to 1.
newtype Distribution assets = Distribution (HomRec assets Share)

-- | Delta between two Distributions. Sums to 0.
newtype DistributionDelta assets = DistributionDelta (HomRec assets ShareDelta)

deriveDistributionInstances
    ''Scalar ''Share ''ShareDelta ''Distribution ''DistributionDelta

-- | Only Values are (Un)Normalizable, because it only makes sense to add up
-- Values held in different Assets.
deriveUnnormalizable ''Scalar ''Distribution ''Value ''Values
deriveUnnormalizable ''Scalar ''DistributionDelta ''ValueDelta ''ValueDeltas
deriveNormalizable ''Scalar ''Distribution ''Value ''Values

instance Labels assets => Truncatable (Distribution assets) where

    truncateTo res (Distribution shares)
        = Distribution $ share <$> xs'' where
            xs'' = addToFirst (one - sum xs') xs' where
                addToFirst
                    :: Scalar
                    -> HomRec assets Scalar
                    -> HomRec assets Scalar
                addToFirst toAdd = \case
                    Empty -> Empty
                    l := x :& lxs -> l := (x + toAdd) :& lxs
            xs' = truncateTo res xs
            xs = unShare <$> shares where
                unShare (Share x) = x

onePoint :: Labels assets => HR.LabelIn assets -> Distribution assets
onePoint assetIn
    = Distribution $ HR.setIn assetIn (share one) $ pure $ share zero

distributionValid :: Labels assets => Distribution assets -> Bool
distributionValid (Distribution shares)
    =  all (>= zero) shares'
    && foldl (+) zero shares' == one where
        shares' = unShare <$> shares where
            unShare (Share scalar) = scalar

distributionDeltaValid :: Labels assets => DistributionDelta assets -> Bool
distributionDeltaValid (DistributionDelta shareDeltas)
    = foldl (+) zero shareDeltas' == zero where
        shareDeltas' = unShareDelta <$> shareDeltas where
            unShareDelta (ShareDelta scalar) = scalar

data OrderAmount
    = Absolute Amount
    | Relative Share
    deriving Show

everything :: OrderAmount
everything = Relative $ share one

absoluteAmount :: Amount -> OrderAmount -> Amount
absoluteAmount totalAmount = \case
    Absolute amount      -> amount
    Relative (Share shr) -> shr .* totalAmount

type TimeStep a = (UTCTime, a)

newtype TimeSeries a = TimeSeries { unTimeSeries :: NonEmpty (TimeStep a) }
    deriving (Show, Functor, Foldable, Traversable, Semigroup)

seriesFromList :: [TimeStep a] -> Maybe (TimeSeries a)
seriesFromList = fmap TimeSeries . nonEmpty
