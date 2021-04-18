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

type Scalar = Fraction Integer

newtype Amount = Amount Scalar deriving (Eq, Ord, Show)
deriveAbsolute ''Scalar ''Amount

newtype AmountDelta = AmountDelta Scalar deriving (Eq, Ord, Show)
deriveRelative ''Scalar ''AmountDelta

deriveDeltaCmp ''Scalar ''Amount ''AmountDelta

newtype Portfolio assets = Portfolio (HomRec Amount assets) deriving (HomRecord Amount)
HR.deriveUnary ''Portfolio [''Show]
HR.deriveAbsolute ''Scalar ''Portfolio

newtype PortfolioDelta assets = PortfolioDelta (HomRec AmountDelta assets)
    deriving (HomRecord AmountDelta)
HR.deriveUnary ''PortfolioDelta [''Show]
HR.deriveRelative ''Scalar ''PortfolioDelta

HR.deriveDelta ''Scalar ''Portfolio ''PortfolioDelta

newtype Price = Price Scalar
newtype Prices assets = Prices (HomRec Price assets)

data OrderAmount
    = Only Amount
    | All

newtype OrderId = OrderId Int
