{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

module Market.Evaluation where

import Data.Map as Map hiding (foldl, foldr)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe
import qualified Data.Ratio as Ratio
import qualified Data.Record.Hom as HomRec
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Control.Monad
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

newtype MetricName = MetricName String
    deriving (Eq, Ord, Semigroup)

deriving newtype instance Show MetricName

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
    downsampled <- downsample (period metric) series
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

newtype InstrumentName = InstrumentName String
    deriving (Eq, Ord)

deriving newtype instance Show InstrumentName

data InstrumentTree a = InstrumentTree
    { self :: a
    , subinstruments :: Map InstrumentName (InstrumentTree a)
    } deriving (Show, Functor, Foldable, Traversable)

type Evaluation = InstrumentTree (Map MetricName (Maybe Double))
type EvaluationOnWindows = InstrumentTree (Map MetricName (TimeSeries Double))

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
integrate series = case maybeXdts of
    Nothing -> snd $ NonEmpty.head txs
    Just xdts -> sum xdts / deltaTime
    where
        maybeXdts = convolve xdt series where
            xdt (t, x) (t', x')
                = (x + x') * 0.5 * timeDiffToDouble (t' `diffUTCTime` t)
        txs = unTimeSeries series
        deltaTime
            = timeDiffToDouble
            $ fst (NonEmpty.last txs) `diffUTCTime` fst (NonEmpty.head txs)
        timeDiffToDouble = fromRational . toRational

calcAvgReturn :: TimeSeries ValuePoint -> Maybe Double
calcAvgReturn = fmap integrate . convolve step where
    step (_, point) (_, point')
        = (retroactive point' - current point) / current point

avgReturn :: NominalDiffTime -> Metric
avgReturn = Metric (MetricName "avgReturn") calcAvgReturn

calcAvgLogReturn :: TimeSeries ValuePoint -> Maybe Double
calcAvgLogReturn = fmap integrate . convolve step where
    step (_, point) (_, point')
        = log $ retroactive point' / current point

avgLogReturn :: NominalDiffTime -> Metric
avgLogReturn = Metric (MetricName "avgLogReturn") calcAvgLogReturn

integrateByPeriod
    :: NominalDiffTime
    -> TimeSeries Double
    -> Maybe (TimeSeries Double)
integrateByPeriod periodLength
    = sequence . fmap (fmap integrate) . intervals periodLength

downsample
    :: NominalDiffTime
    -> TimeSeries a
    -> Maybe (TimeSeries a)
downsample periodLength
    = sequence . fmap (fmap lastInSeries) . intervals periodLength where
        lastInSeries (TimeSeries txs) = snd $ NonEmpty.last txs

periodically
    :: MetricName
    -> NominalDiffTime
    -> (NominalDiffTime -> Metric)
    -> Metric
periodically prefix period metricBuilder
    = metric { name = prefix <> MetricName " " <> name metric } where
        metric = metricBuilder period

hourly :: (NominalDiffTime -> Metric) -> Metric
hourly = periodically (MetricName "hourly") 3600

daily :: (NominalDiffTime -> Metric) -> Metric
daily = periodically (MetricName "daily") $ 3600 * 24

monthly :: (NominalDiffTime -> Metric) -> Metric
monthly = periodically (MetricName "monthly") $ 3600 * 24 * 30.44

evaluate
    :: forall assets c s r
     .  ( HomRec.Labels assets
        , Instrument assets c s
        , Members [Precision, Error String] r
        )
    => [Metric]
    -> TimeSeries (Prices assets)
    -> Portfolio assets
    -> c
    -> Sem r Evaluation
evaluate metrics priceSeries initPortfolio config = do
    maybeTree :: Maybe (InstrumentTree (TimeSeries (PricesPortfolio assets)))
       <- fmap sequence
        $ fmap (fmap $ seriesFromList . reverse)
        $ execState emptyTree
        $ backtest priceSeries initPortfolio config do
            prices <- input @(Prices assets)
            portfolio <- get @(Portfolio assets)
            IState state <- get @(IState s)
            time <- now
            modify @(InstrumentTree [TimeStep (PricesPortfolio assets)])
                $ visit prices portfolio config state visitAgg
                $ visitSelf time

    case maybeTree of
        Just tree
            -> return $ calculateMetrics <$> tree
        Nothing
            -> throw "no trades performed (the price series is too short)"

    where
        emptyTree :: InstrumentTree [TimeStep (PricesPortfolio assets)]
        emptyTree = InstrumentTree [] Map.empty

        visitAgg
            :: AggregateVisitor (TimeStep (PricesPortfolio assets))
                ( InstrumentTree [TimeStep (PricesPortfolio assets)]
               -> InstrumentTree [TimeStep (PricesPortfolio assets)]
                )
        visitAgg timeStep subinstrs tree
            = InstrumentTree (timeStep : self tree)
            $ ($ subinstruments tree)
            $ foldl (.) id
            $ fmap alterFromRec
            $ HomRec.toList subinstrs where
                alterFromRec (label, adjustment) 
                    = alter alteration $ InstrumentName $ show label where
                        alteration = Just . adjustment . maybe emptyTree id

        visitSelf
            :: UTCTime
            -> SelfVisitor assets (TimeStep (PricesPortfolio assets))
        visitSelf time prices portfolio _ _
            = (time, (prices, portfolio))

        calculateMetrics pricePortfolioSeries = Map.fromList $ kv <$> metrics where
            kv metric =
                ( name metric
                , calculateMetric metric pricePortfolioSeries
                )

evaluateOnWindows
    :: forall assets c s r
     .  ( HomRec.Labels assets
        , Instrument assets c s
        , Members [Precision, Error String] r
        )
    => [Metric]
    -> NominalDiffTime
    -> NominalDiffTime
    -> TimeSeries (Prices assets)
    -> Portfolio assets
    -> c
    -> Sem r EvaluationOnWindows
evaluateOnWindows metrics windowLen stride series initPortfolio config = do
    let wnds = windowsE windowLen stride series
    evals <- mapM (evaluateOnWindow <=< fromEither) wnds
    return $ sequenceEvals evals
    where
        evaluateOnWindow window = evaluate metrics window initPortfolio config

        sequenceEvals :: TimeSeries Evaluation -> EvaluationOnWindows
        sequenceEvals = InstrumentTree
            <$> sequenceSelf . fmap self
            <*> sequenceSubinstruments . fmap subinstruments
            where
                sequenceSelf
                    :: TimeSeries (Map MetricName (Maybe Double))
                    -> Map MetricName (TimeSeries Double)
                sequenceSelf
                    = fmap (fromJust . seriesFromList)
                    . foldr (Map.unionWith (++)) Map.empty
                    . fmap mapMaybeToTimeStepList
                    . seriesToList where
                        mapMaybeToTimeStepList
                            :: TimeStep (Map a (Maybe b))
                            -> Map a [TimeStep b]
                        mapMaybeToTimeStepList (time, map)
                            = fmap (fmap (time,) . maybeToList) map

                sequenceSubinstruments
                    :: TimeSeries (Map InstrumentName Evaluation)
                    -> Map InstrumentName EvaluationOnWindows
                sequenceSubinstruments (TimeSeries ((time, map) :| tms))
                    = Map.mapWithKey buildSubtree map where
                        buildSubtree instrName eval
                            = sequenceEvals
                            $ TimeSeries
                            $ (time, eval) :| tes where
                                tes = fmap (onSnd (Map.! instrName)) tms where
                                    onSnd f (x, y) = (x, f y)
