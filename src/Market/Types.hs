{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Market.Types where

import Data.Coerce
import Data.Proxy
import Data.Record.Hom (HomRec, HomRecord, Labels)
import qualified Data.Record.Hom as HR
import GHC.TypeLits
import Market.Deriving
import Numeric.Absolute
import Numeric.Algebra
import Numeric.Delta
import Numeric.Field.Fraction
import Numeric.Kappa
import Numeric.Relative
import Prelude hiding ((*))

type Asset (asset :: Symbol) = Proxy asset

instance (Semiring a, Additive a) => LeftModule a a where
    (.*) = (*)

instance (Semiring a, Additive a) => RightModule a a where
    (*.) = (*)

instance (Semiring a, Additive a) => Module a a

-- | Underlying scalar type.
--
type Scalar = Fraction Integer

-- | Amount of a given Asset.
--
newtype Amount = Amount Scalar
newtype AmountDelta = AmountDelta Scalar

-- | Portfolio is a record of Amounts for each Asset.
--
newtype Portfolio assets = Portfolio (HomRec Amount assets)
newtype PortfolioDelta assets = PortfolioDelta (HomRec AmountDelta assets)

deriveQuantityInstances ''Scalar ''Amount ''AmountDelta ''Portfolio ''PortfolioDelta

-- | Price of a given Asset, measured in units of the baseline asset (e.g. USD).
--
newtype Price = Price Scalar
newtype PriceDelta = PriceDelta Scalar

-- | Record of Prices for each Asset.
--
newtype Prices assets = Prices (HomRec Price assets)
newtype PriceDeltas assets = PriceDeltas (HomRec PriceDelta assets)

deriveQuantityInstances ''Scalar ''Price ''PriceDelta ''Prices ''PriceDeltas

-- | Possessed Value, measured in units of the baseline asset (e.g. USD).
--
newtype Value = Value Scalar
newtype ValueDelta = ValueDelta Scalar

-- | Value held in each Asset.
--
newtype Values assets = Values (HomRec Value assets)
newtype ValueDeltas assets = ValueDeltas (HomRec ValueDelta assets)

deriveQuantityInstances ''Scalar ''Value ''ValueDelta ''Values ''ValueDeltas

-- | Value = Amount * Price.
--
deriveKappaDivision ''Scalar ''Value ''Amount ''Price
deriveKappaDivision ''Scalar ''ValueDelta ''AmountDelta ''PriceDelta
HR.deriveKappa ''Values ''Portfolio ''Prices
HR.deriveKappa ''ValueDeltas ''PortfolioDelta ''PriceDeltas

data OrderAmount
    = Only Amount
    | All

newtype OrderId = OrderId Int
