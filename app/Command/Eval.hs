{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Command.Eval where

import Control.Exception hiding (evaluate)
import Control.Monad
import Data.Fixed
import Data.Foldable
import Data.Proxy
import Data.List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Class as Map
import Data.Maybe
import Data.Text
import qualified Data.Text as Text
import Data.Time
import Data.Time.Clock
import Data.Time.Clock.POSIX
import qualified Dhall
import Market
import Market.Feed.MongoDB
import Market.Instruments
import Market.Notebook
import Market.Types
import Numeric.Field.Fraction
import Options.Applicative
import System.Directory
import System.IO.Error
import Text.Pretty.Simple

import Parser hiding (Parser, fees, metrics)
import qualified Parser as P

data EvalOptions = EvalOptions
    { config :: String
    , initPortfolio :: Portfolio
    , begin :: Maybe UTCTime
    , end :: Maybe UTCTime
    , fees :: Fees
    , metrics :: String
    }

defaultBeginTime :: UTCTime
defaultBeginTime = posixSecondsToUTCTime 0

defaultFees :: Fees
defaultFees = Fees
    { variable = 25 % 100_000
    , fixed = Just ("BNB", Amount $ 15 % 10_000)
    }

evalOptions :: Parser EvalOptions
evalOptions = EvalOptions
    <$> argument str
        ( metavar "CONFIG"
       <> help "Instrument configuration script."
        )
    <*> argument portfolio
        ( metavar "PORTFOLIO"
       <> help "Initial portfolio in format 1 BTC + 0.5 ETH + ..."
        )
    <*> option (Just <$> date)
        ( long "begin"
       <> short 'b'
       <> metavar "DATE"
       <> help
            ( "Begin date in format YYYY-MM-DD. Defaults to the earliest date "
           <> "available."
            )
       <> value Nothing
        )
    <*> option (Just <$> date)
        ( long "end"
       <> short 'e'
       <> metavar "DATE"
       <> help "End date in format YYYY-MM-DD. Defaults to the current date."
       <> value Nothing
        )
    <*> option P.fees
        ( long "fees"
       <> short 'f'
       <> metavar "FEES"
       <> help "Fees in format 0.25% + 0.0015 BNB. Defaults to that value."
       <> value defaultFees
        )
    <*> option str
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

loadBindingsFrom :: FilePath -> IO Text
loadBindingsFrom dir = do
    all <- listDirectory dir
    files <- filterM (doesFileExist . abs) all
    return $ Text.concat $ binding <$> files
    where
        abs = ((dir ++ "/") ++)
        binding name
            = "let " <> pack name <> " = ./" <> pack (abs name) <> "\n"

-- TODO: Windows + output to Vega.
eval :: EvalOptions -> IO ()
eval options = do
    evalBindings <- loadBindingsFrom "./dhall/Market/Evaluation"
    metrics_
       :: [Metric]
       <- Dhall.input Dhall.auto
        $ evalBindings <> "in " <> pack (metrics options)

    defaultEndTime <- getCurrentTime
    let beginTime = maybe defaultBeginTime id $ begin options
        endTime = maybe defaultEndTime id $ end options

    -- TODO: Validation.
    config
        :: SomeInstrumentConfig
        <- Dhall.input Dhall.auto
         $ "./" <> pack (config options) <> " ./dhall/Market/Instrument/Type"

    let assets = nub
            $ managedAssets config
           ++ feeAssets (fees options)
           ++ fmap fst (Map.toList $ initPortfolio options)
        res = Proxy @E6
    priceSeries <- runPriceFeed @Minute res assets beginTime endTime

    pPrint =<< evaluate
        res
        metrics_
        (fees options)
        priceSeries
        (initPortfolio options)
        config
