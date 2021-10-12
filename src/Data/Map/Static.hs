{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Map.Static
    ( module Data.Map.Class
    , StaticMap
    ) where

import Control.DeepSeq
import Data.Functor.Apply
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Class
import Data.Map.Sparse
import qualified Data.Map.Sparse as Sparse
import qualified Data.Traversable.Constrained as Constrained
import Data.Type.Equality
import GHC.Generics
import Numeric.Algebra
import Numeric.Delta
import Numeric.Kappa
import Numeric.Truncatable
import Prelude hiding ((+), lookup, pi)
import Test.QuickCheck
import qualified Prelude

-- | Map with a fixed set of keys: you cannot (cheaply) add or remove keys.
-- Unlike in regular Map, <.> here attempts an outer join and fails in case of
-- missing keys.
newtype StaticMap k v = StaticMap (Map k v)
    deriving (Foldable, Functor, Generic, Traversable)
    deriving anyclass NFData
    deriving newtype (Arbitrary, Eq)

deriving newtype instance Ord k => ReadMap k v (StaticMap k v)
deriving newtype instance Ord k => SetMap k v (StaticMap k v)
deriving newtype instance Ord k => BuildMap k v (StaticMap k v)

instance (Ord k, Show k, Show v) => Show (StaticMap k v) where
    showsPrec = showsPrecReadMap

instance Ord k => Default (StaticMap k v) where
    def = empty

instance Ord k => Apply (StaticMap k) where
    (<.>) = reapplyOuter

instance Constrained.Traversable (StaticMap k) where
    type TraversableConstraint (StaticMap k) f a = ()
    traverse = Prelude.traverse

instance (Ord k, Additive v) => Additive (StaticMap k v) where
    xs + ys = (+) <$> xs <.> ys

instance (Ord k, Abelian v) => Abelian (StaticMap k v)

instance (Ord k, LeftModule a v) => LeftModule a (StaticMap k v) where
    x .* ys = (x .*) <$> ys

instance (Ord k, RightModule a v) => RightModule a (StaticMap k v) where
    xs *. y = (*. y) <$> xs

instance (Ord k, Module a v) => Module a (StaticMap k v)

instance Truncatable v => Truncatable (StaticMap k v) where
    truncateTo = fmap . truncateTo
