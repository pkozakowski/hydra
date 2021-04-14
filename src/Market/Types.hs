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

instance (Semiring a, Additive a) => LeftModule a a where
    (.*) = (*)

instance (Semiring a, Additive a) => RightModule a a where
    (*.) = (*)

instance (Semiring a, Additive a) => Module a a

newtype Amount = Amount (Fraction Integer)
    deriving
        ( Additive
        , Abelian
        , LeftModule Natural
        , RightModule Natural
        , Monoidal
        , LeftModule (Fraction Integer)
        , RightModule (Fraction Integer)
        , Module (Fraction Integer)
        , Show
        )

type Portfolio assets = HomRecord assets Amount

newtype Price = Price (Fraction Integer)
newtype Prices assets = Prices (HomRecord assets Price)

data OrderAmount
    = Only Amount
    | All

newtype OrderId = OrderId Int
