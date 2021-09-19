{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TupleSections #-}

module Market.Ops where

import Control.Exception
import Control.Monad
import Data.Composition hiding ((.*))
import Data.Constraint
import Data.Function
import Data.List.NonEmpty (nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe
import Data.Proxy
import Data.Record.Hom
import Data.Time.Clock
import GHC.TypeLits
import Market.Types
import Numeric.Algebra hiding ((<), (>))
import Numeric.Delta
import Numeric.Kappa
import Numeric.Normalizable
import Prelude hiding ((+), (-), negate, pi)
import qualified Prelude

data ShareTransfer assets
    = ShareTransfer
        { from :: LabelIn assets
        , to :: LabelIn assets
        , share :: Share
        } deriving Show

data BalancingState assets
    = BalancingState
        { diff :: DistributionDelta assets
        , transfers :: [ShareTransfer assets]
        } deriving Show

balancingTransfers
    :: forall assets
     . Labels assets
    => Scalar
    -> Distribution assets
    -> Distribution assets
    -> [ShareTransfer assets]
balancingTransfers tolRel current target
    = transfers $ fix iteration initState where
        iteration go state = if done state then state else go $ next state
        initState = BalancingState diff [] where
            diff = delta current target
        next state = BalancingState
            (diff state + transferDelta transfer)
            (transfer : transfers state) where
                transfer = ShareTransfer maxAssetIn minAssetIn transShare where
                    transShare
                        = fromJust $ fromDelta
                        $ min maxShareDelta $ negate minShareDelta
                    (minAssetIn, minShareDelta) = minDelta $ diff state
                    (maxAssetIn, maxShareDelta) = maxDelta $ diff state
        done state
            = isBalanced tolRel (fromJust $ target `sigma` diff state) target

transferDelta
    :: Labels assets
    => ShareTransfer assets -> DistributionDelta assets
transferDelta (ShareTransfer from to share)
    = DistributionDelta
    $ setIn from (negate $ toDelta share)
    $ setIn to (toDelta share)
    $ zero

isBalanced
    :: Labels assets
    => Scalar -> Distribution assets -> Distribution assets -> Bool
isBalanced _ _ (Distribution Empty) = True
isBalanced tolRel current target@(Distribution targetRec)
    =  maxShareDelta <= tolAbs maxAssetIn
    && minShareDelta >= negate (tolAbs minAssetIn) where
        tolAbs assetIn = tolRel .* (toDelta $ getIn assetIn targetRec) where
        (minAssetIn, minShareDelta) = minDelta $ current `delta` target
        (maxAssetIn, maxShareDelta) = maxDelta $ current `delta` target

minDelta
    :: Labels assets
    => DistributionDelta assets -> (LabelIn assets, ShareDelta)
minDelta (DistributionDelta diff) = argMinimumOn id diff

maxDelta
    :: Labels assets
    => DistributionDelta assets -> (LabelIn assets, ShareDelta)
maxDelta (DistributionDelta diff) = argMinimumOn negate diff

argMinimumOn :: (Labels ls, Ord a) => (a -> a) -> HomRec ls a -> (LabelIn ls, a)
argMinimumOn f r = (assetIn, getIn assetIn r) where
    assetIn = minimumOn $ f . flip getIn r where
        minimumOn f
            = foldl (\x y -> if f x < f y then x else y) (head labels) labels

fromDelta :: ShareDelta -> Maybe Share
fromDelta (ShareDelta x) = if x >= zero then Just $ Share x else Nothing

toDelta :: Share -> ShareDelta
toDelta (Share x) = ShareDelta x

-- | Vector-matrix product between a Distribution and a matrix with
-- Distributions in columns.
redistribute
    :: (Labels ls1, Labels ls2)
    => Distribution ls1 -> HomRec ls1 (Distribution ls2) -> Distribution ls2
redistribute (Distribution vector) matrix
    = Distribution $ fmap Share $ foldl (+) zero
    $ scalarVector <$> vector <*> matrix where
        scalarVector (Share scalar) (Distribution vector)
            = (scalar .*) . unShare <$> vector where
                unShare (Share scalar') = scalar'

totalValue :: Labels assets => Prices assets -> Portfolio assets -> Value
totalValue prices portfolio = foldl (+) zero values where
    Values values = portfolio `pi` prices

valueAllocation
    :: Labels assets
    => Prices assets -> Portfolio assets -> Maybe (Distribution assets)
valueAllocation prices portfolio = normalize $ portfolio `pi` prices

applyFees
    :: Labels assets
    => Fees assets
    -> SomeAmount assets
    -> Maybe (PortfolioDelta assets, Amount)
applyFees fees (asset, amount) = do
    amount' <- amount `sigma` (zero `delta` fixedIfSameAsFrom)
    let amountAfterFees = (one - variable fees) .* amount'
    return (portfolioDelta amount', amountAfterFees)
    where
        fixedIfSameAsFrom = case fixed fees of
            Just (feeAsset, feeAmount)
                -> if feeAsset == asset then feeAmount else zero
            Nothing
                -> zero
        portfolioDelta amount'
            = negate
            $ fixedFee + transfer (asset, variable fees .* amount') where
                fixedFee = case fixed fees of
                    Just someFee -> transfer someFee
                    Nothing -> zero

windows
    :: NominalDiffTime
    -> NominalDiffTime
    -> TimeSeries a
    -> TimeSeries (Maybe (TimeSeries a))
windows length stride (TimeSeries txs)
    = assert (stride > 0)
    $ assert (length >= stride)
    $ fmap (fmap TimeSeries)
    $ TimeSeries
    $ NonEmpty.unfoldr nextWindow (begin, txs) where
        begin = fst $ NonEmpty.head txs
        nextWindow (from, txs')
            = ((to, maybeWindow), (from',) <$> maybeRest) where
                to = length `addUTCTime` from
                from' = stride `addUTCTime` from
                maybeWindow = nonEmpty $ NonEmpty.takeWhile (within length) txs'
                reachedTheEnd = null $ NonEmpty.dropWhile (within length) txs'
                maybeRest = do
                    rest <- nonEmpty $ NonEmpty.dropWhile (within stride) txs'
                    guard $ not reachedTheEnd
                    return rest
                within interval (time, _) = time `diffUTCTime` from < interval

windowsE
    :: NominalDiffTime
    -> NominalDiffTime
    -> TimeSeries a
    -> TimeSeries (Either String (TimeSeries a))
windowsE length = mapWithTime throwOnNothing .: windows length where
    mapWithTime f = TimeSeries . fmap f . unTimeSeries
    throwOnNothing (time, maybeWindow)
        = (time, maybe (Left error) Right maybeWindow) where
            error
                = "empty window "
               ++ show (Prelude.negate length `addUTCTime` time)
               ++ " .. "
               ++ show time

intervals
    :: NominalDiffTime
    -> TimeSeries a
    -> TimeSeries (Maybe (TimeSeries a))
intervals length = windows length length

intervalsE
    :: NominalDiffTime
    -> TimeSeries a
    -> TimeSeries (Either String (TimeSeries a))
intervalsE length = windowsE length length

downsample
    :: NominalDiffTime
    -> TimeSeries a
    -> TimeSeries a
downsample periodLength
    = fromJust
    . catMaybes'
    . fmap (fmap lastInSeries)
    . intervals periodLength where
        catMaybes'
            = seriesFromList . catMaybes . fmap engulf . seriesToList where
                engulf (t, mx) = (t,) <$> mx
        lastInSeries (TimeSeries txs) = snd $ NonEmpty.last txs
