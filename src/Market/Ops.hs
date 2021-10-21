{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TupleSections #-}

module Market.Ops where

import Control.Exception
import Control.Monad
import Data.Composition hiding ((.*))
import Data.Constraint
import Data.Function
import Data.Functor.Apply
import Data.List
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Sparse hiding (Value, null)
import Data.Map.Static hiding (Value, null)
import Data.Maybe
import Data.Time.Clock
import GHC.TypeLits
import Market.Types
import Numeric.Algebra hiding ((<), (>))
import Numeric.Delta
import Numeric.Kappa
import Numeric.Normed
import Prelude hiding ((+), (-), negate, pi)
import qualified Prelude

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

totalValue :: Prices -> Portfolio -> Value
totalValue prices portfolio = foldl (+) zero values where
    Values values = prices `pi` portfolio

valueAllocation :: Prices -> Portfolio -> Maybe (Distribution Asset)
valueAllocation prices portfolio = normalize $ prices `pi` portfolio

applyFees :: Fees -> SomeAmount -> Maybe (PortfolioDelta, Amount)
applyFees fees (asset, amount) = do
    let amountAfterFees = (one - variable fees) .* amount
    return (portfolioDelta, amountAfterFees)
    where
        portfolioDelta
            = negate
            $ fixedFee + transfer (asset, variable fees .* amount) where
                fixedFee = case fixed fees of
                    Just someFee -> transfer someFee
                    Nothing -> zero

absoluteAmount :: Fees -> Asset -> Amount -> OrderAmount -> Amount
absoluteAmount fees asset totalAmount = \case
    Absolute amount      -> amount
    Relative (Share shr) -> shr .* totalAmount' where
        totalAmount' = case fixed fees of
            Just (feeAsset, feeAmount)
                | feeAsset == asset
                    -> case totalAmount `sigma` (zero `delta` feeAmount) of
                        Just totalAmount' -> totalAmount'
                        Nothing -> zero
                | otherwise
                    -> totalAmount
            Nothing -> totalAmount

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

newtype Event k v = Event { changes :: NonEmpty (k, v) }
    deriving (Eq, Ord, Show)

sweep :: Ord k => StaticMap k [TimeStep v] -> [TimeStep (Event k v)]
sweep mapOfSeries =
    if null notNullSeries then []
    else (time, Event changes) : sweep mapOfSeries' where
        notNullSeries = filter (not . null . snd) $ toList mapOfSeries
        time = minimum $ headTime <$> notNullSeries
        changes
            = fromJust
            $ nonEmpty
            $ fmap labelAndHeadValue
            $ filter ((== time) . headTime) notNullSeries where
                labelAndHeadValue (label, series) = (label, snd $ head series)
        headTime = fst . head . snd
        mapOfSeries' = advance <$> mapOfSeries where
            advance series = case series of
                [] -> []
                (t, _) : rest
                    | t == time -> rest
                    | otherwise -> series
