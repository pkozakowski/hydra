module Market.Strategy.Ops where

import Control.Monad
import Data.Function
import Data.List
import Data.Map.Sparse hiding (Value, null)
import Data.Map.Static hiding (Value, null)
import Data.Maybe
import Market
import Market.Ops
import Numeric.Algebra hiding ((<), (>))
import Numeric.Delta
import Numeric.Kappa
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Prelude hiding ((+), (-), negate, pi)

data ShareTransfer
    = ShareTransfer
        { from :: Asset
        , to :: Asset
        , share :: Share
        } deriving Show

data BalancingState
    = BalancingState
        { diff :: DistributionDelta Asset
        , transfers :: [ShareTransfer]
        } deriving Show

balancingTransfers
    :: Scalar -> Distribution Asset -> Distribution Asset -> [ShareTransfer]
balancingTransfers tolRel current target
    = transfers $ fix iteration initState where
        iteration go state = if done state then state else go $ next state
        initState = BalancingState diff [] where
            diff = delta current target
        next state = BalancingState
            (diff state + transferDelta transfer)
            (transfer : transfers state) where
                transfer = ShareTransfer maxAsset minAsset transShare where
                    transShare
                        = fromJust $ fromDelta
                        $ min maxShareDelta $ negate minShareDelta
                    (minAsset, minShareDelta) = minDelta $ diff state
                    (maxAsset, maxShareDelta) = maxDelta $ diff state
        done state
            = isBalanced tolRel (fromJust $ target `sigma` diff state) target

transferDelta :: ShareTransfer -> DistributionDelta Asset
transferDelta (ShareTransfer from to share)
    = DistributionDelta
    $ fromList [(from, negate $ toDelta share), (to, toDelta share)]

isBalanced :: Scalar -> Distribution Asset -> Distribution Asset -> Bool
isBalanced tolRel current target@(Distribution targetMap)
    =  maxShareDelta <= tolAbs maxAsset
    && minShareDelta >= negate (tolAbs minAsset) where
        tolAbs asset = tolRel .* (toDelta $ targetMap ! asset) where
        (minAsset, minShareDelta) = minDelta $ current `delta` target
        (maxAsset, maxShareDelta) = maxDelta $ current `delta` target

minDelta :: DistributionDelta Asset -> (Asset, ShareDelta)
minDelta (DistributionDelta diff) = argMinimumOn id diff

maxDelta :: DistributionDelta Asset -> (Asset, ShareDelta)
maxDelta (DistributionDelta diff) = argMinimumOn negate diff

argMinimumOn :: Ord a => (a -> a) -> SparseMap Asset a -> (Asset, a)
argMinimumOn f m = minimumBy (compare `on` f . snd) $ toList m

fromDelta :: ShareDelta -> Maybe Share
fromDelta (ShareDelta x) = if x >= zero then Just $ Share x else Nothing

toDelta :: Share -> ShareDelta
toDelta (Share x) = ShareDelta x

-- | Vector-matrix product between a Distribution and a matrix with
-- Distributions in columns.
redistribute
    :: forall k
     . Ord k
    => Distribution k -> StaticMap k (Distribution Asset) -> Distribution Asset
redistribute (Distribution vector) matrix
    = Distribution $ fmap Share $ foldl (+) zero
    $ scalarVector <$> vector `reapply` matrix where
        scalarVector (Share scalar) (Distribution vector)
            = (scalar .*) . unShare <$> vector where
                unShare (Share scalar') = scalar'
        reapply = reapplyOuter @_ @_ @(StaticMap k (SparseMap Asset Scalar))
        infixl 4 `reapply`

allocationToTrades
    :: forall r
     . Members
        [ Market
        , Input Portfolio
        , Input Prices
        , Input Fees
        , Error MarketError
        ] r
    => Scalar -> Distribution Asset -> Sem r ()
allocationToTrades tolerance targetAlloc
    = do
        prices <- input @Prices
        portfolio <- input @Portfolio
        let value = totalValue prices portfolio
        when (value > zero) do
            let currentAlloc = fromJust $ valueAllocation prices portfolio
                transfers
                    = balancingTransfers tolerance currentAlloc targetAlloc
            sequence_ $ transferToTrade value <$> transfers
    where
        transferToTrade value (ShareTransfer from to shr) = do
            prices <- input @Prices @r
            portfolio' <- input @Portfolio
            let balance = portfolio' ! from
                amount = fromJust $ value `kappa` (prices ! from)
            -- Convert from share in the portfolio to share in the balance of
            -- a specific asset.
            orderAmount <- case amount `pi` shr `kappa` balance of
                Just shr' -> return
                    -- balance can be lower than amount `pi` shr
                    -- because of fixed fees, so we clip.
                    $ Relative $ min shr' $ Share one
                Nothing -> throw
                    $ InsufficientBalanceForTransfer
                        (from, amount) portfolio'

            when (not $ orderAmountIsZero orderAmount)
                $ trade from to orderAmount
                    `catch` \case
                        -- Can't afford the fees => skip this trade -
                        -- Strategies don't have to check that.
                        InsufficientBalanceToCoverFees _ _ -> return ()
                        -- Don't have enough money for the transfer => throw -
                        -- Strategies shouldn't exceed the balance.
                        e -> throw e
