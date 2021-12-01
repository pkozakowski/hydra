module Command.Tune where

import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Lazy as L
import Data.Time
import qualified Dhall
import Market
import Market.Feed.MongoDB
import Market.Instrument
import Market.Notebook hiding (duration)
import qualified Market.Notebook as Notebook
import Market.Tuning hiding (tune)
import Options.Applicative
import Polysemy
import System.Random
import Text.Pretty.Simple

import Command.Eval hiding (core)
import Parser hiding (Parser, fees, metrics)

data TuneOptions = TuneOptions
    { grid :: String
    , metric :: String
    , stopWhen :: StopWhen
    , core :: CoreEvalOptions
    }

tuneOptions :: Parser TuneOptions
tuneOptions = TuneOptions
    <$> argument str
        ( metavar "CONFIG"
       <> help "Instrument configuration script."
        )
    <*> option str
        ( long "metric"
       <> short 'm'
       <> metavar "METRIC"
       <> help
            ( "A metric in Dhall, type: ./dhall/Market/Evaluation/Metric. "
           <> "Can be supplied using the syntax `period metric`, where period "
           <> "is one of {hourly, daily, monthly}, and metric is one of the "
           <> "metrics defined in Market.Evaluation. Defaults to hourly "
           <> "avgReturn."
            )
       <> value "hourly avgReturn"
        )
    <*> ( TrialLimit <$> option auto
            ( long "trials"
           <> short 't'
           <> metavar "N"
           <> help "Number of tried configurations."
            )
      <|> TimeLimit <$> option duration
            ( long "time"
           <> short 'T'
           <> metavar "DURATION"
           <> help
                ( "Time limit for tuning in format 1.4s (for seconds; use "
               <> "m/h/d/M for higher units)."
                )
            )
        )
    <*> coreEvalOptions

tune :: TuneOptions -> IO ()
tune options = Dhall.detailed do
    let coreOptions = core options

    evalBindings <- loadBindingsFrom "./dhall/Market/Evaluation"
    metric_
       :: Metric
       <- Dhall.input Dhall.auto
        $ evalBindings <> "in " <> pack (metric options)

    grid
        :: Grid SomeInstrumentConfig
        <- Dhall.input Dhall.auto
         $ "./" <> pack (grid options) <> " ./dhall/Market/Tuning/Grid"

    (beginTime, endTime) <- beginEndTimes (begin coreOptions) (end coreOptions)
    let fees_ = fees coreOptions
        initPortfolio_ = initPortfolio coreOptions
        assetsToPrices assets
            = embed
            $ runPriceFeed @Minute resolutionP assets' beginTime endTime where
                assets' = allAssets fees_ initPortfolio_ assets
    fitness <- case window coreOptions of
        Nothing -> return
            $ instrumentFitness metric_ fees_ assetsToPrices initPortfolio_
        Just window -> do
            stride_ <- strideDuration window $ stride coreOptions
            return $ instrumentFitnessOnWindows metric_ fees_ window stride_
                assetsToPrices initPortfolio_

    gen <- newStdGen
    (bestConfig, bestFitness) <- Notebook.tune
        resolutionP (stopWhen options) fitness (runGridRandom gen) grid
    putStrLn
        $ unpack
        $ "best config [fitness = " <> pack (show bestFitness) <> "]:\n"
       <> L.toStrict (pShow bestConfig)
