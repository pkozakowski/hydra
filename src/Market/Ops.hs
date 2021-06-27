{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Market.Ops where

import Data.Coerce
import Data.Constraint
import Data.Function
import Data.Maybe
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
        { diff :: DistributionDelta assets
        , transfers :: [ShareTransfer assets]
        } deriving Show

balancingTransfers
    :: forall (assets :: [Symbol]). Labels assets
    => Scalar -> Distribution assets -> Distribution assets -> [ShareTransfer assets]
balancingTransfers tolRel current target
    = transfers $ fix (\go state -> if done state then state else go $ next state) initState where
        initState = BalancingState diff [] where
            diff = delta current target
        next state = BalancingState applyTransfer $ transfer : transfers state where
            applyTransfer
                = coerce
                $ setIn maxLi (maxS - transS)
                $ setIn minLi (minS + transS)
                $ coerce
                $ diff state
            transfer = ShareTransfer maxLi minLi $ coerce transS
            (minLi, minS) = minDelta $ diff state
            (maxLi, maxS) = maxDelta $ diff state
            transS = min maxS $ negate minS
        done state = isBalanced tolRel (fromJust $ target `sigma` diff state) target

isBalanced :: Labels assets => Scalar -> Distribution assets -> Distribution assets -> Bool
isBalanced _ _ (Distribution Empty) = True
isBalanced tolRel current target
    = maxShareDelta <= tolAbs maxAssetIn && minShareDelta >= negate (tolAbs minAssetIn) where
        tolAbs assetIn = tolRel .* (getIn assetIn $ coerce target)
        (minAssetIn, minShareDelta) = minDelta $ current `delta` target
        (maxAssetIn, maxShareDelta) = maxDelta $ current `delta` target

minDelta :: Labels assets => DistributionDelta assets -> (LabelIn assets, ShareDelta)
minDelta = argMinimumOn id . coerce

maxDelta :: Labels assets => DistributionDelta assets -> (LabelIn assets, ShareDelta)
maxDelta = argMinimumOn negate . coerce

argMinimumOn :: (Labels ls, Ord a) => (a -> a) -> HomRec ls a -> (LabelIn ls, a)
argMinimumOn f r = (assetIn, getIn assetIn r) where
    assetIn = minimumOn $ f . flip getIn r where
        minimumOn f = foldl (\x y -> if f x < f y then x else y) (head labels) labels
