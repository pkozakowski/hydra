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

import Control.Monad
import Control.Parallel.Strategies
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
import qualified Data.Record.Hom as HR
import Data.Semigroup.Traversable
import Data.String
import Data.Time.Clock
import Data.Time.Clock.POSIX
import GHC.Generics
import Market
import Market.Internal.Sem
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

data ValueChange = ValueChange
    { previous :: Double
    , current  :: Double
    }

type PricesPortfolio assets = (Prices assets, Portfolio assets)

type ValueChangeCalculator assets
   = PricesPortfolio assets
  -> PricesPortfolio assets
  -> ValueChange

-- | Active value calculation takes into account changes in the portfolio.
-- This is the most honest evaluation mode (modulo the future leak, but it's
-- negligible here), but doesn't make sense for nested Instruments - their
-- portfolios are "virtual" - calculated on-the-fly based on the current
-- prices.
activeVC :: HR.Labels assets => ValueChangeCalculator assets
activeVC (prices, portfolio) (prices', portfolio') = ValueChange
    { previous = toDouble $ totalValue prices portfolio
    , current = toDouble $ totalValue prices' portfolio'
    }

-- | Passive value calculation doesn't take into account changes in the
-- portfolio, so it measures only the robustness of the current portfolio
-- to the future price changes. Makes sense for nested Instruments, because
-- the "virtual" nested portfolios are not recalculated based on the new prices.
passiveVC :: HR.Labels assets => ValueChangeCalculator assets
passiveVC (prices, _) (prices', portfolio') = ValueChange
    { previous = toDouble $ totalValue prices portfolio'
    , current = toDouble $ totalValue prices' portfolio'
    }

toDouble :: Value -> Double
toDouble (Value x)
    = fromRational $ numerator x Ratio.% denominator x

data Metric = Metric
    { name :: MetricName
    , calculate :: TimeSeries ValueChange -> Double
    , period :: NominalDiffTime
    }

calculateMetric
    :: HR.Labels assets
    => ValueChangeCalculator assets
    -> Metric
    -> TimeSeries (PricesPortfolio assets)
    -> Maybe Double
calculateMetric vcc metric series = do
    let downsampled = downsample (period metric) series
    valueChanges <- convolve step downsampled
    return $ calculate metric valueChanges
    where
        step (_, pp) (_, pp') = vcc pp pp'

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

data Evaluation' res = Evaluation
    { active :: Map MetricName res
    , passive :: InstrumentTree (Map MetricName res)
    } deriving (Functor, Generic, NFData, Show)

instance Apply Evaluation' where

    fs <.> xs = Evaluation
        { active = active fs <.> active xs
        , passive = getCompose $ Compose (passive fs) <.> Compose (passive xs)
        }

type Evaluation = Evaluation' Double
type EvaluationOnWindows = Evaluation' (TimeSeries Double)

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

calcAvgReturn :: TimeSeries ValueChange -> Double
calcAvgReturn = integrate . fmap step where
    step change = (current change - previous change) / current change

avgReturn :: NominalDiffTime -> Metric
avgReturn = Metric "avgReturn" calcAvgReturn

calcAvgLogReturn :: TimeSeries ValueChange -> Double
calcAvgLogReturn = integrate . fmap step where
    step change = log $ current change / previous change

avgLogReturn :: NominalDiffTime -> Metric
avgLogReturn = Metric "avgLogReturn" calcAvgLogReturn

integrateByPeriod
    :: NominalDiffTime
    -> TimeSeries Double
    -> Maybe (TimeSeries Double)
integrateByPeriod periodLength
    = sequence . fmap (fmap integrate) . intervals periodLength

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
     .  ( HR.Labels assets
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
       <- fmap (fmap sequence1 . seriesFromList . fst)
        $ runOutputList
        $ backtest priceSeries initPortfolio config do
            prices <- input @(Prices assets)
            portfolio <- get @(Portfolio assets)
            IState state <- get @(IState s)
            time <- now
            output @(TimeStep (InstrumentTree (PricesPortfolio assets)))
                $ (time,)
                $ visit prices portfolio config state visitAgg
                $ visitSelf

    case maybeTree of
        Just tree -> return $ Evaluation
            { active  = calculateMetrics activeVC (self tree)
            , passive = calculateMetrics passiveVC <$> tree
            }
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
            $ HR.toList subinstrs where
                onFst f (x, y) = (f x, y)

        visitSelf :: SelfVisitor assets (PricesPortfolio assets)
        visitSelf prices portfolio _ _
            = (prices, portfolio)

        calculateMetrics vcc pricePortfolioSeries
            = Map.fromList $ catMaybes $ kv <$> metrics where
                kv metric = (,)
                    <$> Just (name metric)
                    <*> calculateMetric vcc metric pricePortfolioSeries

evaluateOnWindows
    :: forall assets c s r
     .  ( HR.Labels assets
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
    truncator <- getTruncator
    let interpreter = runError . runPrecisionFromTruncator truncator
        deinterpreter = either (throw @String) return
    evals <- pforSem interpreter deinterpreter wnds
        $ evaluateOnWindow <=< fromEither
    return $ sequence1 evals
    where
        evaluateOnWindow window
            = evaluate metrics window initPortfolio config
