{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Traversable.Constrained where

import Data.Constraint
import Data.Map
import Prelude hiding (Traversable, mapM, sequence, sequenceA, traverse)
import qualified Prelude
import Test.QuickCheck
import Test.QuickCheck.Classes

class (Functor t, Foldable t) => Traversable t where

    type TraversableConstraint
        (t :: * -> *) (f :: * -> *) (a :: *) :: Constraint

    traverse
        ::  ( Applicative f
            , TraversableConstraint t f a
            , TraversableConstraint t f b
            , TraversableConstraint t f (f b)
            )
        => (a -> f b) -> t a -> f (t b)
    traverse fn = sequenceA . fmap fn

    sequenceA
        ::  ( Applicative f
            , TraversableConstraint t f a
            , TraversableConstraint t f (f a)
            )
        => t (f a) -> f (t a)
    sequenceA = traverse id

    mapM 
        ::  ( Monad m
            , TraversableConstraint t m a
            , TraversableConstraint t m b
            , TraversableConstraint t m (m b)
            )
        => (a -> m b) -> t a -> m (t b)
    mapM = traverse

    sequence
        ::  ( Monad m
            , TraversableConstraint t m a
            , TraversableConstraint t m (m a)
            )
        => t (m a) -> m (t a)
    sequence = sequenceA

instance Traversable (Map k) where
    type TraversableConstraint (Map k) f a = ()
    traverse = Prelude.traverse
