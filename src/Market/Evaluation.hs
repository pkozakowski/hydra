{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module Market.Evaluation where

import Data.Hashable
import Data.HashMap as Map
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
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Output
import Polysemy.State

newtype MetricName = MetricName String
    deriving (Hashable, Eq, Ord, Semigroup)

deriving newtype instance Show MetricName

data Metric = Metric
    { name :: MetricName
    , calculate :: TimeSeries Double -> Maybe Double
    }

newtype InstrumentName = InstrumentName String
    deriving (Hashable, Eq, Ord)

deriving newtype instance Show InstrumentName

data InstrumentTree a = InstrumentTree
    { self :: a
    , subinstruments :: Map InstrumentName (InstrumentTree a)
    } deriving (Show, Functor, Foldable, Traversable)

type Evaluation = InstrumentTree (Map MetricName (Maybe Double))

convolve
    :: (TimeStep Double -> TimeStep Double -> Double)
    -> TimeSeries Double
    -> Maybe (TimeSeries Double)
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

calcAvgReturn :: TimeSeries Double -> Maybe Double
calcAvgReturn = fmap integrate . convolve step where
    step (_, x) (_, x') = (x' - x) / x

avgReturn :: Metric
avgReturn = Metric (MetricName "avgReturn") calcAvgReturn

calcAvgLogReturn :: TimeSeries Double -> Maybe Double
calcAvgLogReturn = fmap integrate . convolve step where
    step (_, x) (_, x') = log $ x' / x

avgLogReturn :: Metric
avgLogReturn = Metric (MetricName "avgLogReturn") calcAvgLogReturn

integrateByPeriod
    :: NominalDiffTime
    -> TimeSeries Double
    -> Maybe (TimeSeries Double)
integrateByPeriod periodLength
    = seriesFromList
    . fmap integrateGroup
    . NonEmpty.groupWith (periodIndex . fst)
    . unTimeSeries where
        integrateGroup txs = (beginTime, integrate $ TimeSeries txs) where
            beginTime
                = posixSecondsToUTCTime
                $ fromInteger (periodIndex headTime) * periodLength where
                    headTime = fst $ NonEmpty.head txs
        periodIndex t = floor $ utcTimeToPOSIXSeconds t / periodLength

periodically :: MetricName -> NominalDiffTime -> Metric -> Metric
periodically prefix periodLength (Metric name calculate)
    = Metric 
        { name = prefix <> MetricName " " <> name
        , calculate = calculate <=< integrateByPeriod periodLength
        }

hourly :: Metric -> Metric
hourly = periodically (MetricName "hourly") 3600

daily :: Metric -> Metric
daily = periodically (MetricName "daily") $ 3600 * 24

monthly :: Metric -> Metric
monthly = periodically (MetricName "monthly") $ 3600 * 24 * 30.44

evaluate
    :: forall assets c s r
     .  ( HomRec.Labels assets
        , Instrument assets c s
        , Member (Error String) r
        )
    => TimeSeries (Prices assets)
    -> Portfolio assets
    -> c
    -> [Metric]
    -> Sem r Evaluation
evaluate priceSeries initPortfolio config metrics = do
    maybeValueTree
       <- fmap sequence
        $ fmap (fmap $ seriesFromList . reverse)
        $ execState emptyTree
        $ backtest priceSeries initPortfolio config do
            prices <- input @(Prices assets)
            portfolio <- get @(Portfolio assets)
            IState state <- get @(IState s)
            time <- now
            modify @(InstrumentTree [TimeStep Value])
                $ visit config state portfolio visitAgg
                $ visitSelf time prices 

    case maybeValueTree of
        Just valueTree
            -> return $ calculateMetrics <$> valueTree
        Nothing
            -> throw "no trades performed (the price series is too short)"

    where
        emptyTree :: InstrumentTree [TimeStep Value]
        emptyTree = InstrumentTree [] Map.empty

        visitAgg
            :: AggregateVisitor (TimeStep Value)
                ( InstrumentTree [TimeStep Value]
               -> InstrumentTree [TimeStep Value]
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
            -> Prices assets
            -> SelfVisitor assets (TimeStep Value)
        visitSelf time prices _ _ portfolio
            = (time, totalValue prices portfolio)

        calculateMetrics valueSeries = Map.fromList $ kv <$> metrics where
            kv metric =
                ( name metric
                , calculate metric $ toDouble <$> valueSeries
                ) where
                    toDouble (Value x)
                        = fromRational $ numerator x Ratio.% denominator x
                    unValue (Value x) = x
