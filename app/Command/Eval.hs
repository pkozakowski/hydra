{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}

module Command.Eval where

import Control.Exception hiding (evaluate)
import Control.Monad
import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty hiding (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Fixed
import Data.Foldable
import Data.List
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Class ((!))
import qualified Data.Map.Class as Map
import Data.Maybe
import Data.Proxy
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text
import Data.Time
import Data.Time.Clock
import Data.Time.Clock.POSIX
import qualified Dhall
import Graphics.Vega.VegaLite hiding (name, window)
import Market
import Market.Instrument
import Market.Notebook hiding (duration)
import Market.Plot
import Market.Types
import Numeric.Field.Fraction
import Options.Applicative
import Parser hiding (Parser, fees, metrics)
import qualified Parser as P
import System.Directory
import System.IO.Error

data EvalOptions = EvalOptions
  { config :: String,
    metrics :: String,
    plot :: Maybe String,
    core :: CoreEvalOptions
  }

data CoreEvalOptions = CoreEvalOptions
  { initPortfolio :: Portfolio,
    begin :: Maybe UTCTime,
    end :: Maybe UTCTime,
    window :: Maybe NominalDiffTime,
    stride :: Double,
    fees :: Fees
  }

defaultBeginTime :: UTCTime
defaultBeginTime = posixSecondsToUTCTime 0

defaultFees :: Fees
defaultFees =
  Fees
    { variable = 25 % 100_000,
      fixed = Just ("BNB", Amount $ 15 % 10_000)
    }

evalOptions :: Parser EvalOptions
evalOptions =
  EvalOptions
    <$> argument
      str
      ( metavar "CONFIG"
          <> help "Instrument configuration script."
      )
    <*> option
      str
      ( long "metrics"
          <> short 'm'
          <> metavar "METRICS"
          <> help
            ( "List of metrics in Dhall, type: "
                <> "List ./dhall/Market/Evaluation/Metric. Can be supplied using "
                <> "the syntax [period metric, ...], where period is one of "
                <> "{hourly, daily, monthly}, and metric is one of the metrics "
                <> "defined in Market.Evaluation. Defaults to [hourly avgReturn]."
            )
          <> value "[hourly avgReturn]"
      )
    <*> option
      (Just <$> str)
      ( long "plot"
          <> short 'p'
          <> metavar "FILE"
          <> help
            ( "Will render an evaluation plot to that file, in VegaLite "
                <> "HTML."
            )
          <> value Nothing
      )
    <*> coreEvalOptions

coreEvalOptions =
  CoreEvalOptions
    <$> argument
      portfolio
      ( metavar "PORTFOLIO"
          <> help "Initial portfolio in format 1 BTC + 0.5 ETH + ..."
      )
    <*> option
      (Just <$> date)
      ( long "begin"
          <> short 'b'
          <> metavar "DATE"
          <> help
            ( "Begin date in format YYYY-MM-DD. Defaults to the earliest "
                <> "date available."
            )
          <> value Nothing
      )
    <*> option
      (Just <$> date)
      ( long "end"
          <> short 'e'
          <> metavar "DATE"
          <> help
            ( "End date in format YYYY-MM-DD. Defaults to the current "
                <> "date."
            )
          <> value Nothing
      )
    <*> option
      (Just <$> duration)
      ( long "window"
          <> short 'w'
          <> metavar "DURATION"
          <> help
            ( "Window size in format 1.4s (for seconds; use m/h/d/M for "
                <> "higher units). Specifying turns on the windowed evaluation "
                <> "mode."
            )
          <> value Nothing
      )
    <*> option
      float
      ( long "stride"
          <> short 's'
          <> metavar "FRACTION"
          <> help
            ( "The time difference between two consecutive windows. "
                <> "Specified relatively to the window size. Defaults to 0.5."
            )
          <> value 0.5
      )
    <*> option
      P.fees
      ( long "fees"
          <> short 'f'
          <> metavar "FEES"
          <> help "Fees in format 0.25% + 0.0015 BNB. Defaults to that value."
          <> value defaultFees
      )

loadBindingsFrom :: FilePath -> IO Text
loadBindingsFrom dir = do
  all <- listDirectory dir
  files <- filterM (doesFileExist . abs) all
  return $ Text.concat $ binding <$> files
  where
    abs = ((dir ++ "/") ++)
    binding name =
      "let " <> pack name <> " = ./" <> pack (abs name) <> "\n"

encodePretty :: ToJSON a => a -> BS.ByteString
encodePretty = encodePretty' defConfig {confCompare = compare}

allAssets :: Fees -> Portfolio -> [Asset] -> [Asset]
allAssets fees initPortfolio managedAssets =
  nub $
    managedAssets
      ++ feeAssets fees
      ++ fmap fst (Map.toList initPortfolio)

resolutionP :: Proxy E6
resolutionP = Proxy

beginEndTimes :: Maybe UTCTime -> Maybe UTCTime -> IO (UTCTime, UTCTime)
beginEndTimes maybeBeginTime maybeEndTime = do
  defaultEndTime <- getCurrentTime
  let beginTime = maybe defaultBeginTime id maybeBeginTime
      endTime = maybe defaultEndTime id maybeEndTime
  return (beginTime, endTime)

strideDuration :: NominalDiffTime -> Double -> IO NominalDiffTime
strideDuration window stride = do
  when (stride <= 0 || stride > 1) $
    fail $
      "stride should be in range (0, 1]; got "
        ++ show stride
  return $ realToFrac stride * window

-- eval :: EvalOptions -> IO ()
-- eval options = do
--  let coreOptions = core options
--
--  evalBindings <- loadBindingsFrom "./dhall/Market/Evaluation"
--  metrics_ ::
--    [Metric] <-
--    Dhall.input Dhall.auto $
--      evalBindings <> "in " <> pack (metrics options)
--
--  -- TODO: Validation.
--  config ::
--    SomeInstrumentConfig <-
--    Dhall.input Dhall.auto $
--      "./" <> pack (config options) <> " ./dhall/Market/Instrument/Type"
--
--  let assets =
--        allAssets (fees coreOptions) (initPortfolio coreOptions) $
--          managedAssets config
--  (beginTime, endTime) <- beginEndTimes (begin coreOptions) (end coreOptions)
--  priceSeries <- runPriceFeed @Minute resolutionP assets beginTime endTime
--
--  case window coreOptions of
--    Nothing -> do
--      when (isJust $ plot options) $
--        fail "plotting only supported for evaluation on windows"
--      evaluation <-
--        evaluate
--          resolutionP
--          metrics_
--          (fees coreOptions)
--          priceSeries
--          (initPortfolio coreOptions)
--          config
--      BS.putStrLn $ encodePretty evaluation
--    Just window -> do
--      stride_ <- strideDuration window $ stride coreOptions
--      evaluation <-
--        evaluateOnWindows
--          resolutionP
--          metrics_
--          (fees coreOptions)
--          window
--          stride_
--          priceSeries
--          (initPortfolio coreOptions)
--          config
--
--      case plot options of
--        Just path -> do
--          let plot = plotEvaluation evaluation
--          toHtmlFile path $ toVegaLite plot
--        Nothing -> return ()
--
--      BS.putStrLn $ encodePretty evaluation
