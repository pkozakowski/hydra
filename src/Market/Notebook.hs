module Market.Notebook
  ( module Evaluation,
    module Tuning,
    evaluate,
    evaluateOnWindows,
    tune,
  )
where

import Control.Logging
import Data.Composition
import Data.Fixed
import Data.List.NonEmpty as NonEmpty
import Data.Text
import qualified Data.Text.Lazy as L
import Data.Time.Clock
import Data.Time.Clock.POSIX
import qualified Dhall.Pretty as Dh
import Market
import Market.Dhall
import Market.Evaluation hiding (evaluate, evaluateOnWindows)
import qualified Market.Evaluation as Evaluation
import Market.Feed
import Market.Internal.IO
import Market.Time
import Market.Tuning hiding (tune)
import qualified Market.Tuning as Tuning
import Market.Types
import Numeric.Precision
import Polysemy
import Polysemy.Error
import Polysemy.Output
import Prelude hiding (log)

evaluate ::
  forall c s res r.
  ( Instrument c s,
    HasResolution res
  ) =>
  res ->
  [Metric] ->
  Fees ->
  TimeSeries Prices ->
  Portfolio ->
  c ->
  IO Evaluation
evaluate res =
  semToIOPure
    . mapError @MarketError show
    . runPrecision res
    .::. Evaluation.evaluate

evaluateOnWindows ::
  forall c s res r.
  ( Instrument c s,
    HasResolution res
  ) =>
  res ->
  [Metric] ->
  Fees ->
  NominalDiffTime ->
  NominalDiffTime ->
  TimeSeries (Prices) ->
  Portfolio ->
  c ->
  IO EvaluationOnWindows
evaluateOnWindows res =
  semToIOPure
    . mapError @MarketError show
    . runPrecision res
    .:::. Evaluation.evaluateOnWindows

tune ::
  forall c s res e.
  (Instrument c s, Show c, HasResolution res, Show e) =>
  res ->
  StopWhen ->
  ( c ->
    Sem
      [ Output (c, Double),
        Precision,
        Time,
        Error e,
        Embed IO
      ]
      Double
  ) ->
  (Grid c -> NonEmpty c) ->
  Grid c ->
  IO (c, Double)
tune res =
  semToIO
    . mapError @e show
    . raiseUnder
    . runTimeIO
    . runPrecision res
    . runOutputLog
    .:: Tuning.tune
  where
    runOutputLog = runOutputSem \(config, ftn) ->
      embed $
        log $
          "new best config [fitness = "
            <> pack (show ftn)
            <> "]:\n"
            <> pack (showPrettyExpr $ smartToDhall config)
