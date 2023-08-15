{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Map.Static
  ( module Data.Map.Class
  , StaticMap
  ) where

import Control.DeepSeq
import Data.Aeson
import Data.Functor.Apply
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.Class
import Data.Map.Sparse
import Data.Map.Sparse qualified as Sparse
import Data.MessagePack
import Data.Traversable.Constrained qualified as Constrained
import Data.Type.Equality
import Dhall (FromDhall)
import GHC.Generics
import Numeric.Algebra
import Numeric.Delta
import Numeric.Kappa
import Numeric.Truncatable
import Test.QuickCheck
import Prelude hiding (lookup, pi, (+))
import Prelude qualified

{- | Map with a fixed set of keys: you cannot (cheaply) add or remove keys.
Unlike in regular Map, <.> here attempts an outer join and fails in case of
missing keys.
-}
newtype StaticMap k v = StaticMap (Map k v)
  deriving (Foldable, Functor, Generic, Traversable)
  deriving anyclass (FromDhall, NFData, ToJSON)
  deriving newtype (Arbitrary, Eq, MessagePack)

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
