{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Market.Types where

import Data.Map
import Numeric.Algebra
import Numeric.Field.Fraction
import Prelude hiding ((+), (*), map)

data Asset
    = BTC
    | ETH
    | ADA
    deriving (Eq, Ord, Show)

newtype Amount = Amount (Fraction Integer)
    deriving
        ( Additive
        , Multiplicative
        , Abelian
        , Semiring
        , LeftModule Natural
        , RightModule Natural
        )

instance LeftModule (Fraction Integer) Amount where
    x .* Amount y = Amount $ x * y

instance RightModule (Fraction Integer) Amount where
    Amount x *. y = Amount $ x * y

instance Module (Fraction Integer) Amount

newtype Price = Price (Fraction Integer)

newtype AssetPortfolio = AssetPortfolio (Map Asset Amount)

instance Additive AssetPortfolio where
    AssetPortfolio p1 + AssetPortfolio p2 = AssetPortfolio $ unionWith (+) p1 p2

instance (Semiring a, LeftModule a Amount) => LeftModule a AssetPortfolio where
    n .* AssetPortfolio p = AssetPortfolio $ map (n .*) p

instance (Semiring a, RightModule a Amount) => RightModule a AssetPortfolio where
    AssetPortfolio p *. n = AssetPortfolio $ map (*. n) p

instance Monoidal AssetPortfolio where
    zero = AssetPortfolio empty

instance Module (Fraction Integer) AssetPortfolio

newtype AssetPrices = AssetPrices (Map Asset Price)

data OrderAmount
    = Only Amount
    | All

newtype OrderId = OrderId Int
