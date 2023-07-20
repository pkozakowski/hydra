{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant $" #-}

module Market.Evaluation where

import Control.Monad
import Control.Parallel.Strategies
import Data.Aeson (ToJSON, ToJSONKey)
import Data.Bifunctor
import Data.Composition
import Data.Foldable hiding (toList)
import Data.Functor.Apply
import Data.Functor.Compose
import Data.List hiding (uncons)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Static hiding (Value)
import Data.Maybe
import Data.Monoid
import Data.Ratio qualified as Ratio
import Data.Semigroup.Traversable
import Data.String
import Data.Text (pack, unpack)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Dhall (FromDhall)
import Dhall qualified as Dh
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

newtype MetricName = MetricName {unMetricName :: String}
  deriving newtype (Eq, NFData, IsString, Ord, Semigroup, Show, ToJSONKey)

data ValueChange = ValueChange
  { previous :: Double
  , current :: Double
  }

type PricesPortfolio = (Prices, Portfolio)

type ValueChangeCalculator =
  PricesPortfolio
  -> PricesPortfolio
  -> ValueChange

{- | Active value calculation takes into account changes in the portfolio.
This is the most honest evaluation mode (modulo the future leak, but it's
negligible here), but doesn't make sense for nested Instruments - their
portfolios are "virtual" - calculated on-the-fly based on the current
prices.
-}
activeVC :: ValueChangeCalculator
activeVC (prices, portfolio) (prices', portfolio') =
  ValueChange
    { previous = toDouble $ totalValue prices portfolio
    , current = toDouble $ totalValue prices' portfolio'
    }

{- | Passive value calculation doesn't take into account changes in the
portfolio, so it measures only the robustness of the current portfolio
to the future price changes. Makes sense for nested Instruments, because
the "virtual" nested portfolios are not recalculated based on the new prices.
-}
passiveVC :: ValueChangeCalculator
passiveVC (prices, _) (prices', portfolio') =
  ValueChange
    { previous = toDouble $ totalValue prices portfolio'
    , current = toDouble $ totalValue prices' portfolio'
    }

toDouble :: Value -> Double
toDouble (Value x) =
  fromRational $ numerator x Ratio.% denominator x

data Period = Period
  { duration :: NominalDiffTime
  , periodName :: MetricName
  }

instance FromDhall Period where
  autoWith _ = Dh.Decoder {..}
    where
      expected = Dh.expected nameDecoder

      extract expr =
        Period
          <$> Dh.extract Dh.auto expr
          <*> Dh.extract nameDecoder expr

      nameDecoder =
        Dh.union
          ( (buildName "secondly" <$> Dh.constructor "Seconds" Dh.natural)
              <> (buildName "minutely" <$> Dh.constructor "Minutes" Dh.natural)
              <> (buildName "hourly" <$> Dh.constructor "Hours" Dh.natural)
              <> (buildName "daily" <$> Dh.constructor "Days" Dh.natural)
              <> (buildName "monthly" <$> Dh.constructor "Months" Dh.natural)
          )
        where
          buildName name number =
            if number == 1
              then name
              else MetricName (show number) <> "-" <> name

data Metric = Metric
  { name :: MetricName
  , calculate :: TimeSeries ValueChange -> Double
  , period :: NominalDiffTime
  }

instance FromDhall Metric where
  autoWith _ =
    Dh.record
      ( buildMetric
          <$> Dh.field "period" Dh.auto
          <*> Dh.field "calculator" decodeMetricSansPeriod
      )
    where
      buildMetric period =
        periodically (periodName period) (duration period)
      decodeMetricSansPeriod =
        Dh.union $
          foldMap cons configurableMetricsSansPeriod
        where
          cons (name, calc) =
            Dh.constructor (pack name) (calc <$ Dh.unit)

calculateMetric
  :: ValueChangeCalculator
  -> Metric
  -> TimeSeries PricesPortfolio
  -> Maybe Double
calculateMetric vcc metric series = do
  let upsampled = upsample (period metric) series
  valueChanges <- convolveDilated (period metric) step upsampled
  return $ calculate metric valueChanges
  where
    step (_, pp) (_, pp') = vcc pp pp'

data InstrumentTree a = InstrumentTree
  { self :: a
  , subinstruments :: StaticMap InstrumentName (InstrumentTree a)
  }
  deriving (Functor, Foldable, Generic, NFData, Show, Traversable, ToJSON)

instance Apply InstrumentTree where
  fs <.> xs =
    InstrumentTree
      { self = self fs $ self xs
      , subinstruments =
          getCompose $
            Compose (subinstruments fs) <.> Compose (subinstruments xs)
      }

data Evaluation' res = Evaluation
  { active :: StaticMap MetricName res
  , passive :: InstrumentTree (StaticMap MetricName res)
  }
  deriving (Functor, Generic, NFData, Show, ToJSON)

instance Apply Evaluation' where
  fs <.> xs =
    Evaluation
      { active = active fs <.> active xs
      , passive = getCompose $ Compose (passive fs) <.> Compose (passive xs)
      }

type Evaluation = Evaluation' Double
type EvaluationOnWindows = Evaluation' (TimeSeries Double)

flattenTree :: InstrumentTree a -> [(InstrumentName, a)]
flattenTree = flattenWithPrefix ""
  where
    flattenWithPrefix prefix tree =
      (prefix, self tree) : subinstrs
      where
        subinstrs =
          uncurry flattenWithPrefix . extendPrefix
            =<< toList (subinstruments tree)
        extendPrefix (instrName, tree')
          | prefix == "" = (instrName, tree')
          | otherwise = (prefix <> "." <> instrName, tree')

calcReturns :: TimeSeries ValueChange -> TimeSeries Double
calcReturns =
  fmap \change ->
    (current change - previous change) / current change

avgReturn :: NominalDiffTime -> Metric
avgReturn = Metric "avgReturn" $ integrate . calcReturns

calcLogReturns :: TimeSeries ValueChange -> TimeSeries Double
calcLogReturns = fmap \change -> log $ current change / previous change

avgLogReturn :: NominalDiffTime -> Metric
avgLogReturn = Metric "avgLogReturn" $ integrate . calcLogReturns

calcStdReturn :: TimeSeries ValueChange -> Double
calcStdReturn = calcStdReturn' . calcReturns

calcStdReturn' :: TimeSeries Double -> Double
calcStdReturn' returns =
  sqrt $ integrate $ (** 2) . (mean -) <$> returns
  where
    mean = integrate returns

stdReturn :: NominalDiffTime -> Metric
stdReturn = Metric "stdReturn" calcStdReturn

calcSharpeRatio :: TimeSeries ValueChange -> Double
calcSharpeRatio changes = integrate $ (/ std) <$> returns
  where
    returns = calcReturns changes
    std = calcStdReturn' returns

sharpeRatio :: NominalDiffTime -> Metric
sharpeRatio = Metric "sharpeRatio" calcSharpeRatio

type MetricSansPeriod = NominalDiffTime -> Metric

periodically
  :: MetricName
  -> NominalDiffTime
  -> MetricSansPeriod
  -> Metric
periodically prefix period metricBuilder =
  metric {name = prefix <> " " <> name metric}
  where
    metric = metricBuilder period

secondly :: MetricSansPeriod -> Metric
secondly = periodically "secondly" 60

minutely :: MetricSansPeriod -> Metric
minutely = periodically "minutely" 60

hourly :: MetricSansPeriod -> Metric
hourly = periodically "hourly" 3600

daily :: MetricSansPeriod -> Metric
daily = periodically "daily" $ 3600 * 24

monthly :: MetricSansPeriod -> Metric
monthly = periodically "monthly" $ 3600 * 24 * 30.44

configurableMetricsSansPeriod :: [(String, MetricSansPeriod)]
configurableMetricsSansPeriod =
  [ ("AvgReturn", avgReturn)
  , ("AvgLogReturn", avgLogReturn)
  , ("StdReturn", stdReturn)
  , ("SharpeRatio", sharpeRatio)
  ]

evaluate
  :: forall c s r
   . ( Instrument c s
     , Members [Precision, Error MarketError] r
     )
  => [Metric]
  -> Fees
  -> TimeSeries Prices
  -> Portfolio
  -> c
  -> Sem r Evaluation
evaluate metrics fees priceSeries initPortfolio config = do
  maybeTree :: Maybe (InstrumentTree (TimeSeries PricesPortfolio)) <-
    fmap (fmap sequence1 . seriesFromList . fst) $
      runOutputList $
        backtest fees priceSeries initPortfolio config do
          prices <- input @Prices
          portfolio <- get @Portfolio
          IState state <- get @(IState s)
          time <- now
          output @(TimeStep (InstrumentTree PricesPortfolio)) $
            (time,) $
              visit prices portfolio config state visitAgg visitSelf

  case maybeTree of
    Just tree ->
      return $
        Evaluation
          { active = calculateMetrics activeVC (self tree)
          , passive = calculateMetrics passiveVC <$> tree
          }
    Nothing ->
      throw @MarketError $
        OtherError "no trades performed (price series is too short)"
  where
    visitAgg
      :: AggregateVisitor
          PricesPortfolio
          (InstrumentTree PricesPortfolio)
    visitAgg pricesPortfolio subinstrs =
      InstrumentTree pricesPortfolio $
        fromList (first (InstrumentName . show) <$> toList subinstrs)

    visitSelf :: SelfVisitor PricesPortfolio
    visitSelf prices portfolio _ _ =
      (prices, portfolio)

    calculateMetrics vcc pricePortfolioSeries =
      fromList $ mapMaybe kv metrics
      where
        kv metric =
          (,)
            <$> Just (name metric)
            <*> calculateMetric vcc metric pricePortfolioSeries

evaluateOnWindows
  :: forall c s r
   . ( Instrument c s
     , Members [Precision, Error MarketError] r
     )
  => [Metric]
  -> Fees
  -> NominalDiffTime
  -> NominalDiffTime
  -> TimeSeries Prices
  -> Portfolio
  -> c
  -> Sem r EvaluationOnWindows
evaluateOnWindows metrics fees windowLen stride series initPortfolio config = do
  let wnds = windowsE windowLen stride series
  truncator <- getTruncator
  let interpreter = runError . runPrecisionFromTruncator truncator
      deinterpreter = either (throw @MarketError) return
  evals <-
    pforSem interpreter deinterpreter wnds $
      evaluateOnWindow
        <=< fromEither @MarketError . first OtherError
  return $ sequence1 evals
  where
    evaluateOnWindow window =
      evaluate metrics fees window initPortfolio config
