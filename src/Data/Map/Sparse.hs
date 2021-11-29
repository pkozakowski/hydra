{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Map.Sparse
    ( module Data.Map.Class
    , Default (..)
    , SparseMap
    ) where

import Control.Applicative hiding (empty)
import Control.DeepSeq
import Data.Functor.Apply
import Data.Map.Class
import Data.Map.Default hiding (def)
import qualified Data.Traversable.Constrained as Constrained
import Data.Type.Equality
import Dhall (FromDhall)
import qualified Dhall as Dh
import GHC.Generics
import Numeric.Algebra
import Numeric.Algebra.Deriving
import Numeric.Delta
import Numeric.Kappa
import Numeric.Truncatable
import Prelude hiding ((+), (-), lookup, null, pi)
import Test.QuickCheck

-- | Like DefaultMap, but with the default fixed for every type.
newtype SparseMap k v = SparseMap { unSparseMap :: DefaultMap k v }
    deriving (Eq, Foldable, Functor)
    deriving newtype
        ( Apply, Generic, NFData, Show
        , Abelian, Additive
        , Truncatable
        )

class Default a where
    def :: a

instance Default Int where
    def = 0

instance Default Natural where
    def = 0

instance Default a => Default (Maybe a) where
    def = pure def

instance Ord k => ReadMap k v (SparseMap k v) where
    lookup key = lookup key . unSparseMap
    toList = toList . unSparseMap

deriving newtype instance Ord k => SetMap k v (SparseMap k v)
deriving newtype instance Ord k => ReadWriteMap k v (SparseMap k v)

instance (Ord k, Default v) => BuildMap k v (SparseMap k v) where
    fromList entries = SparseMap $ DefaultMap def $ fromList entries

instance (Ord k, Default v) => Default (SparseMap k v) where
    def = empty

-- | We don't have full Traversable, because it would need to traverse over
-- the default element, which would make it incompatible with the Foldable
-- instance. In turn, we don't want Foldable to fold over the default element,
-- because that would make the semantics different from the regular Map.
instance Ord k => Constrained.Traversable (SparseMap k) where

    type TraversableConstraint (SparseMap k) f a = Default a

    sequenceA
        = fmap (SparseMap . DefaultMap def)
        . sequenceA
        . contents
        . unSparseMap

deriving newtype instance (Ord k, Default v, LeftModule a v)
    => LeftModule a (SparseMap k v)
deriving newtype instance (Ord k, Default v, RightModule a v)
    => RightModule a (SparseMap k v)
deriving newtype instance (Ord k, Default v, Module a v)
    => Module a (SparseMap k v)

-- Assumes that def coincides with zero.
instance (Ord k, Default v, Monoidal v) => Monoidal (SparseMap k v) where
    zero = empty

instance (Ord k, Default v, Group v) => Group (SparseMap k v) where
    xs - ys = (-) <$> xs <.> ys

instance (Ord k, Default a, Default b, Delta a b)
    => Delta (SparseMap k a) (SparseMap k b) where

    delta xs ys = delta <$> xs <.> ys
    sigma xs ys = Constrained.sequenceA $ sigma <$> xs <.> ys

instance (Ord k, Default b, Default c, Kappa a b c)
    => Kappa (SparseMap k a) (SparseMap k b) (SparseMap k c) where

    kappa xs ys = Constrained.sequenceA $ kappa <$> xs <.> ys
    kappa' xs zs = Constrained.sequenceA $ kappa' <$> xs <.> zs
    pi ys zs = pi <$> ys <.> zs

instance (Ord k, Arbitrary k, Default v, Arbitrary v)
    => Arbitrary (SparseMap k v) where

    arbitrary = SparseMap . DefaultMap def <$> arbitrary
    shrink m = fmap fromList $ shrink $ toList m

instance (Ord k, FromDhall k, Default v, FromDhall v)
    => FromDhall (SparseMap k v) where

    autoWith normalizer
        = SparseMap . DefaultMap def <$> Dh.autoWith normalizer
