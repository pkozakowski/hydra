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
import Numeric.Absolute
import Numeric.Algebra
import Numeric.Delta
import Numeric.Field.Fraction
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
newtype Amount = Amount Scalar deriving (Eq, Ord, Show)
newtype AmountDelta = AmountDelta Scalar deriving (Eq, Ord, Show)
deriveAbsolute ''Scalar ''Amount
deriveRelative ''Scalar ''AmountDelta
deriveDeltaCmp ''Scalar ''Amount ''AmountDelta

-- | Portfolio is a record of Amounts for each Asset.
--
newtype Portfolio assets = Portfolio (HomRec Amount assets) deriving (HomRecord Amount)
newtype PortfolioDelta assets = PortfolioDelta (HomRec AmountDelta assets)
    deriving (HomRecord AmountDelta)
HR.deriveUnary ''Portfolio [''Show]
HR.deriveUnary ''PortfolioDelta [''Show]
HR.deriveAbsolute ''Scalar ''Portfolio
HR.deriveRelative ''Scalar ''PortfolioDelta
HR.deriveDelta ''Scalar ''Portfolio ''PortfolioDelta

-- | Price of a given Asset.
--
newtype Price = Price Scalar deriving (Eq, Ord, Show)
newtype PriceDelta = PriceDelta Scalar deriving (Eq, Ord, Show)
deriveAbsolute ''Scalar ''Price
deriveRelative ''Scalar ''PriceDelta
deriveDeltaCmp ''Scalar ''Price ''PriceDelta

-- | Record of Prices for each Asset, measured in units of the baseline asset (e.g. USD).
--
newtype Prices assets = Prices (HomRec Price assets) deriving (HomRecord Price)
newtype PriceDeltas assets = PriceDeltas (HomRec PriceDelta assets)
    deriving (HomRecord PriceDelta)
HR.deriveUnary ''Prices [''Show]
HR.deriveUnary ''PriceDeltas [''Show]
HR.deriveAbsolute ''Scalar ''Prices
HR.deriveRelative ''Scalar ''PriceDeltas
HR.deriveDelta ''Scalar ''Prices ''PriceDeltas

-- | Possessed Value, measured in units of the baseline asset (e.g. USD).
--
newtype Value = Value Scalar deriving (Eq, Ord, Show)
newtype ValueDelta = ValueDelta Scalar deriving (Eq, Ord, Show)
deriveAbsolute ''Scalar ''Value
deriveRelative ''Scalar ''ValueDelta
deriveDeltaCmp ''Scalar ''Value ''ValueDelta

-- | Record of Values held in each Asset.
--
newtype Values assets = Values (HomRec Value assets) deriving (HomRecord Value)
newtype ValueDeltas assets = ValueDeltas (HomRec ValueDelta assets)
    deriving (HomRecord ValueDelta)
HR.deriveUnary ''Values [''Show]
HR.deriveUnary ''ValueDeltas [''Show]
HR.deriveAbsolute ''Scalar ''Values
HR.deriveRelative ''Scalar ''ValueDeltas
HR.deriveDelta ''Scalar ''Values ''ValueDeltas

data OrderAmount
    = Only Amount
    | All

newtype OrderId = OrderId Int
