{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Market.Types where

import Data.Proxy
import Data.Record.Hom
import GHC.TypeLits
import Numeric.Algebra
import Numeric.Field.Fraction
import Prelude hiding ((*))

type Asset (asset :: Symbol) = Proxy asset

newtype Amount = Amount (Fraction Integer)
    deriving
        ( Additive
        , Multiplicative
        , Abelian
        , Semiring
        , LeftModule Natural
        , RightModule Natural
        , Monoidal
        , Show
        )

instance LeftModule (Fraction Integer) Amount where
    x .* Amount y = Amount $ x * y

instance RightModule (Fraction Integer) Amount where
    Amount x *. y = Amount $ x * y

instance Module (Fraction Integer) Amount

newtype Price = Price (Fraction Integer) 

type Portfolio assets = HomRecord assets Amount

newtype Prices assets = Prices (HomRecord assets Price)

data OrderAmount
    = Only Amount
    | All

newtype OrderId = OrderId Int
