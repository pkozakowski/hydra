{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Market.Ops where

import Data.Coerce
import Data.Constraint
import Data.Function
import Data.Proxy
import Data.Record.Hom
import GHC.TypeLits
import Market.Types
import Numeric.Algebra hiding ((<))
import Numeric.Delta
import Prelude hiding ((+), (-), negate)

data ShareTransfer (assets :: [Symbol])
    = ShareTransfer
        { from :: LabelIn assets
        , to :: LabelIn assets
        , share :: Share
        } deriving Show

data BalancingState (assets :: [Symbol])
    = BalancingState
        { diff :: HomRec assets ShareDelta
        , transfers :: [ShareTransfer assets]
        } deriving Show

balancingTransfers
    :: forall (assets :: [Symbol]). Labels assets
    => Scalar -> Distribution assets -> Distribution assets -> [ShareTransfer assets]
balancingTransfers tolerance current target
    = transfers $ fix (\go state -> if done state then state else go $ next state) initState where
        initState = BalancingState diff [] where
            DistributionDelta diff = delta current target
        next state = BalancingState applyTransfer $ transfer : transfers state where
            applyTransfer
                = setIn maxLi (maxS - transS) $ setIn minLi (minS + transS) $ diff state where
            transfer = ShareTransfer maxLi minLi $ coerce transS
            (minLi, minS) = minDelta state
            (maxLi, maxS) = maxDelta state
            transS = min maxS $ negate minS
        done (BalancingState Empty _) = True
        done state = maxS <= toleranceAbs maxLi && minS >= negate (toleranceAbs minLi) where
            toleranceAbs li = tolerance .* (getIn li $ coerce target) :: ShareDelta
            (minLi, minS) = minDelta state
            (maxLi, maxS) = maxDelta state
        minDelta = argMinimumOn id . diff
        maxDelta = argMinimumOn negate . diff
        argMinimumOn f r = (ai, getIn ai r) where
            ai = minimumOn $ f . flip getIn r
        minimumOn f = foldl (\x y -> if f x < f y then x else y) (head ais) ais
        ais = fst <$> toList (coerce target :: HomRec assets Share)
