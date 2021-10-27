{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Map.Default
    ( module Data.Map.Class
    , DefaultMap (..)
    ) where

import Control.DeepSeq
import Data.Functor.Apply
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Class
import Dhall (FromDhall)
import GHC.Generics
import Numeric.Algebra
import Numeric.Delta
import Numeric.Truncatable
import Prelude hiding ((+), (-), (*), lookup, pi)
import Test.QuickCheck

-- | Map with a default value. lookup always returns Just.
data DefaultMap k v = DefaultMap
    { def :: v
    , contents :: Map k v
    } deriving (FromDhall, Functor, Generic, NFData)

instance Ord k => ReadMap k v (DefaultMap k v) where

    lookup key map = case Map.lookup key $ contents map of
        Just value -> Just value
        Nothing -> Just $ def map

    toList = toList . contents

instance Ord k => SetMap k v (DefaultMap k v) where
    set = insert

instance Ord k => ReadWriteMap k v (DefaultMap k v) where

    insert key value map
        = DefaultMap (def map) $ insert key value $ contents map

    delete key map
        = DefaultMap (def map) $ delete key $ contents map

instance Ord k => Foldable (DefaultMap k) where
    foldMap f map = foldMap f $ contents map

instance Ord k => Apply (DefaultMap k) where

    fs <.> as
        = DefaultMap (def fs $ def as)
        $ Map.unions [inner, left, right] where
            inner = contents fs <.> contents as
            left = def fs <$> contents as
            right = ($ def as) <$> contents fs

instance (Ord k, Eq v) => Eq (DefaultMap k v) where
    xs == ys = def xs == def ys && (all id $ (==) <$> xs <.> ys)

instance (Ord k, Show k, Show v) => Show (DefaultMap k v) where
    showsPrec = showsPrecReadMap

instance (Ord k, Additive v) => Additive (DefaultMap k v) where
    xs + ys = (+) <$> xs <.> ys

instance (Ord k, Abelian v) => Abelian (DefaultMap k v)

instance (Ord k, LeftModule a v) => LeftModule a (DefaultMap k v) where
    x .* ys = (x .*) <$> ys

instance (Ord k, RightModule a v) => RightModule a (DefaultMap k v) where
    xs *. y = (*. y) <$> xs

instance (Ord k, Module a v) => Module a (DefaultMap k v)

instance Truncatable v => Truncatable (DefaultMap k v) where
    truncateTo = fmap . truncateTo

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (DefaultMap k v) where
    arbitrary = DefaultMap <$> arbitrary <*> arbitrary
    shrink m = DefaultMap <$> shrink (def m) <*> shrink (contents m)
