{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Market.Types where

import Data.Proxy
import Data.Record.Hom
import GHC.TypeLits
import Numeric.Absolute
import Numeric.Algebra
import Numeric.Field.Fraction
import Numeric.Relative
import Prelude hiding ((*))

type Asset (asset :: Symbol) = Proxy asset

instance (Semiring a, Additive a) => LeftModule a a where
    (.*) = (*)

instance (Semiring a, Additive a) => RightModule a a where
    (*.) = (*)

instance (Semiring a, Additive a) => Module a a

type Scalar = Fraction Integer

newtype Amount = Amount Scalar deriving (Eq, Ord, Show)
deriveAbsolute ''Scalar ''Amount

newtype AmountDelta = AmountDelta Scalar deriving (Eq, Ord, Show)
deriveRelative ''Scalar ''AmountDelta

type Portfolio assets = HomRecord assets Amount

newtype Price = Price Scalar
newtype Prices assets = Prices (HomRecord assets Price)

data OrderAmount
    = Only Amount
    | All

newtype OrderId = OrderId Int
