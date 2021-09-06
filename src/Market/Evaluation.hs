{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

module Market.Evaluation where

import Control.DeepSeq
import Control.Monad
import Data.Composition
import Data.Foldable
import Data.Functor.Apply
import Data.Functor.Compose
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.List hiding (uncons)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe
import qualified Data.Ratio as Ratio
import qualified Data.Record.Hom as HomRec
import Data.Semigroup.Traversable
import Data.String
import Data.Time.Clock
import Data.Time.Clock.POSIX
import GHC.Generics
import Market
import Market.Ops
import Market.Simulation
import Market.Time
import Market.Types
import Numeric.Field.Fraction
import Numeric.Precision
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Output
import Polysemy.State

newtype MetricName = MetricName { unMetricName :: String }
    deriving newtype (Eq, NFData, IsString, Ord, Semigroup, Show)

data ValuePoint = ValuePoint
    { current :: Double     -- Current prices, current portfolio.
    , retroactive :: Double -- Current prices, previous portfolio.
    }

data Metric = Metric
    { name :: MetricName
    , calculate :: TimeSeries ValuePoint -> Maybe Double
    , period :: NominalDiffTime
    }

type PricesPortfolio assets = (Prices assets, Portfolio assets)

calculateMetric
    :: HomRec.Labels assets
    => Metric
    -> TimeSeries (PricesPortfolio assets)
    -> Maybe Double
calculateMetric metric series = do
    let downsampled = downsample (period metric) series
    valuePoints <- convolve step downsampled
    calculate metric valuePoints
    where
        step (_, (_, portfolio)) (_, (prices', portfolio'))
            = ValuePoint
                { current = toDouble $ totalValue prices' portfolio'
                , retroactive = toDouble $ totalValue prices' portfolio
                } where
                    toDouble (Value x)
                        = fromRational $ numerator x Ratio.% denominator x

newtype InstrumentName = InstrumentName { unInstrumentName :: String }
    deriving newtype (Eq, NFData, IsString, Ord, Semigroup, Show)

data InstrumentTree a = InstrumentTree
    { self :: a
    , subinstruments :: (Map InstrumentName (InstrumentTree a))
    } deriving (Functor, Foldable, Generic, NFData, Show, Traversable)

instance Apply InstrumentTree where

    fs <.> xs = InstrumentTree
        { self = self fs $ self xs
        , subinstruments
            = getCompose
            $ Compose (subinstruments fs) <.> Compose (subinstruments xs)
        }

type Evaluation = InstrumentTree (Map MetricName Double)
type EvaluationOnWindows = InstrumentTree (Map MetricName (TimeSeries Double))

flattenTree :: InstrumentTree a -> [(InstrumentName, a)]
flattenTree = flattenWithPrefix $ "" where
    flattenWithPrefix prefix tree
        = [(prefix, self tree)] ++ subinstrs where
            subinstrs
                = uncurry flattenWithPrefix . extendPrefix
              =<< Map.toList (subinstruments tree)
            extendPrefix (instrName, tree')
                | prefix == "" = (instrName, tree')
                | otherwise = (prefix <> "." <> instrName, tree')

convolve
    :: (TimeStep a -> TimeStep a -> b)
    -> TimeSeries a
    -> Maybe (TimeSeries b)
convolve f series
    = case unTimeSeries series of
        _ :| [] -> Nothing
        tx :| txs
           -> seriesFromList
            $ zip (fst <$> txs)
            $ uncurry f <$> zip (tx : txs) txs

-- | Integration using the trapezoidal rule.
integrate :: TimeSeries Double -> Double
integrate series = case convolve xdt series of
    Nothing -> startX
    Just (TimeSeries txdts)
        -> finish $ foldMap' ((,) <$> Last . Just . fst <*> Sum . snd) txdts
    where
        xdt (t, x) (t', x')
            = (x + x') * 0.5 * timeDiffToDouble (t' `diffUTCTime` t)
        (startTime, startX) = NonEmpty.head $ unTimeSeries series
        finish (Last (Just endTime), Sum sxdt) = sxdt / deltaTime where
            deltaTime = timeDiffToDouble $ endTime `diffUTCTime` startTime
        timeDiffToDouble = fromRational . toRational

calcAvgReturn :: TimeSeries ValuePoint -> Maybe Double
calcAvgReturn = fmap integrate . convolve step where
    step (_, point) (_, point')
        = (retroactive point' - current point) / current point

avgReturn :: NominalDiffTime -> Metric
avgReturn = Metric "avgReturn" calcAvgReturn

calcAvgLogReturn :: TimeSeries ValuePoint -> Maybe Double
calcAvgLogReturn = fmap integrate . convolve step where
    step (_, point) (_, point')
        = log $ retroactive point' / current point

avgLogReturn :: NominalDiffTime -> Metric
avgLogReturn = Metric "avgLogReturn" calcAvgLogReturn

integrateByPeriod
    :: NominalDiffTime
    -> TimeSeries Double
    -> Maybe (TimeSeries Double)
integrateByPeriod periodLength
    = sequence . fmap (fmap integrate) . intervals periodLength

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

periodically
    :: MetricName
    -> NominalDiffTime
    -> (NominalDiffTime -> Metric)
    -> Metric
periodically prefix period metricBuilder
    = metric { name = prefix <> " " <> name metric } where
        metric = metricBuilder period

hourly :: (NominalDiffTime -> Metric) -> Metric
hourly = periodically "hourly" 3600

daily :: (NominalDiffTime -> Metric) -> Metric
daily = periodically "daily" $ 3600 * 24

monthly :: (NominalDiffTime -> Metric) -> Metric
monthly = periodically "monthly" $ 3600 * 24 * 30.44

evaluate
    :: forall assets c s r
     .  ( HomRec.Labels assets
        , Instrument assets c s
        , Members [Precision, Error String] r
        )
    => [Metric]
    -> NominalDiffTime
    -> TimeSeries (Prices assets)
    -> Portfolio assets
    -> c
    -> Sem r Evaluation
evaluate metrics samplePeriod priceSeries initPortfolio config = do
    let priceSeries' = downsample samplePeriod priceSeries

    maybeTree :: Maybe (InstrumentTree (TimeSeries (PricesPortfolio assets)))
       <- fmap (fmap sequence1 . seriesFromList . fst)
        $ runOutputList
        $ backtest priceSeries' initPortfolio config do
            prices <- input @(Prices assets)
            portfolio <- get @(Portfolio assets)
            IState state <- get @(IState s)
            time <- now
            output @(TimeStep (InstrumentTree (PricesPortfolio assets)))
                $ (time,)
                $ visit prices portfolio config state visitAgg
                $ visitSelf

    case maybeTree of
        Just tree
            -> return $ calculateMetrics <$> tree
        Nothing
            -> throw @String "no trades performed (the price series is too short)"

    where
        visitAgg
            :: AggregateVisitor (PricesPortfolio assets)
                (InstrumentTree (PricesPortfolio assets))
        visitAgg pricesPortfolio subinstrs
            = InstrumentTree pricesPortfolio
            $ Map.fromList
            $ fmap (onFst $ InstrumentName . show)
            $ HomRec.toList subinstrs where
                onFst f (x, y) = (f x, y)

        visitSelf :: SelfVisitor assets (PricesPortfolio assets)
        visitSelf prices portfolio _ _
            = (prices, portfolio)

        calculateMetrics pricePortfolioSeries
            = Map.fromList $ catMaybes $ kv <$> metrics where
                kv metric = (,)
                    <$> Just (name metric)
                    <*> calculateMetric metric pricePortfolioSeries

evaluateOnWindows
    :: forall assets c s r
     .  ( HomRec.Labels assets
        , Instrument assets c s
        , Members [Precision, Error String] r
        )
    => [Metric]
    -> NominalDiffTime
    -> NominalDiffTime
    -> NominalDiffTime
    -> TimeSeries (Prices assets)
    -> Portfolio assets
    -> c
    -> Sem r EvaluationOnWindows
evaluateOnWindows
    metrics samplePeriod windowLen stride series initPortfolio config = do
        let wnds = windowsE windowLen stride series
        -- mapM' and force are crucial; without them, the intermediate results
        -- of evals on all windows are put in the memory all at once, causing
        -- a huge leak.
        evals <- mapM' (fmap force . evaluateOnWindow <=< fromEither) wnds
        return $ sequenceEvals evals
        where
            evaluateOnWindow window
                = evaluate metrics samplePeriod window initPortfolio config
            sequenceEvals = getCompose . sequence1 . fmap Compose
            mapM' action (TimeSeries (tx :| txs))
                = fromJust . seriesFromList <$> go (tx : txs) where
                    go = \case
                        (t, x) : txs -> do
                            !y <- action x
                            tys <- go txs
                            return $ (t, y) : tys
                        [] -> return []
