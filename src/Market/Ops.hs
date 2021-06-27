{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Market.Ops where

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
        next state = BalancingState
            (applyTransfer transfer $ diff state)
            (transfer : transfers state) where
                transfer = ShareTransfer maxAssetIn minAssetIn transShare where
                    transShare = fromJust $ fromDelta $ min maxShareDelta $ negate minShareDelta
                    (minAssetIn, minShareDelta) = minDelta $ diff state
                    (maxAssetIn, maxShareDelta) = maxDelta $ diff state
        done state = isBalanced tolRel (fromJust $ target `sigma` diff state) target

applyTransfer :: ShareTransfer assets -> DistributionDelta assets -> DistributionDelta assets
applyTransfer (ShareTransfer from to share) (DistributionDelta diff)
    = DistributionDelta
    $ setIn from (balance from - toDelta share)
    $ setIn to (balance to + toDelta share)
    $ diff where
        balance = flip getIn diff

isBalanced :: Labels assets => Scalar -> Distribution assets -> Distribution assets -> Bool
isBalanced _ _ (Distribution Empty) = True
isBalanced tolRel current target@(Distribution targetRec)
    = maxShareDelta <= tolAbs maxAssetIn && minShareDelta >= negate (tolAbs minAssetIn) where
        tolAbs assetIn = tolRel .* (toDelta $ getIn assetIn targetRec)
        (minAssetIn, minShareDelta) = minDelta $ current `delta` target
        (maxAssetIn, maxShareDelta) = maxDelta $ current `delta` target

minDelta :: Labels assets => DistributionDelta assets -> (LabelIn assets, ShareDelta)
minDelta (DistributionDelta diff) = argMinimumOn id diff

maxDelta :: Labels assets => DistributionDelta assets -> (LabelIn assets, ShareDelta)
maxDelta (DistributionDelta diff) = argMinimumOn negate diff

argMinimumOn :: (Labels ls, Ord a) => (a -> a) -> HomRec ls a -> (LabelIn ls, a)
argMinimumOn f r = (assetIn, getIn assetIn r) where
    assetIn = minimumOn $ f . flip getIn r where
        minimumOn f = foldl (\x y -> if f x < f y then x else y) (head labels) labels

fromDelta :: ShareDelta -> Maybe Share
fromDelta (ShareDelta x) = if x >= zero then Just $ Share x else Nothing

toDelta :: Share -> ShareDelta
toDelta (Share x) = ShareDelta x
