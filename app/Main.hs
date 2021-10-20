{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Logging
import Data.Fixed
import Data.Time.Clock
import Market.Feed.MongoDB
import Market.Instruments
import Market.Notebook
import Market.Types
import Numeric.Algebra
import Numeric.Field.Fraction
import Options.Applicative hiding (helper, hsubparser)
import System.Random
import Text.Pretty.Simple

import Command.Eval
import Command.Sync
import Help

data Cmd
    = Eval EvalOptions
    | Sync SyncOptions

parseCmd :: Parser Cmd
parseCmd
    = Eval <$> hsubparser
        ( command "eval"
            $ info evalOptions
            $ progDesc "Evaluate an instrument."
        )
  <|> Sync <$> hsubparser
        ( command "sync"
            $ info syncOptions
            $ progDesc "Synchronize the price data."
        )

data Verbosity = Warning | Info | Debug
    deriving (Enum)

toLogLevel :: Verbosity -> LogLevel
toLogLevel = \case
    Warning -> LevelWarn
    Info    -> LevelInfo
    Debug   -> LevelDebug

data Options = Options
    { cmd :: Cmd
    , verbosity :: Verbosity
    }

options :: Parser Options
options = Options
    <$> parseCmd
    <*> ( toEnum . length <$> many
            ( flag' ()
                ( long "verbose"
               <> short 'v'
               <> help
                    ( "Enable verbose mode. By default, only WARNINGs and ERRORs "
                   <> "are shown. -v enables INFO messages, -vv enables DEBUG "
                   <> "messages."
                    )
                )
            )
        )

run :: Options -> IO ()
run opts = withStderrLogging do
    setLogLevel $ toLogLevel $ verbosity opts
    case cmd opts of
        Sync opts' -> sync opts'
        Eval opts' -> eval opts'

main :: IO ()
main = run =<< customExecParser (prefs showHelpOnEmpty) opts where
    opts = info (options <**> helper) idm

{-

type Assets = ["BTCB", "ETH", "WBNB"]

prec = Proxy @E6

metrics = [hourly avgReturn] :: [Metric]

fees = Fees
    { fixed = Just $ gwei @"WBNB" 150000
    , variable = 3 % 1000
    }

config = BalanceConfig
    { configs
        = Proxy @"BTCB" := Hold (Proxy @"BTCB")
       ~& Proxy @"ETH" := Hold (Proxy @"ETH")
       ~& Proxy @"WBNB" := Hold (Proxy @"WBNB")
       ~& noConfigs
    , target = Distribution
        $ Proxy @"BTCB" := Share (1 % 3)
       :& Proxy @"ETH" := Share (1 % 3)
       :& Proxy @"WBNB" := Share (1 % 3)
       :& Empty
    , tolerance = 1 % 10
    , updateEvery = 0 -- nominalDay Prelude./ 240
    } :: BalanceConfig Assets Assets

grid = BalanceConfig
    <$> pure (configs config)
    <*> pure (target config)
    <*> choice [1 % 1000, 1 % 300, 1 % 100, 1 % 30, 1 % 10, 1 % 3]  -- tolerance
    <*> choice [0, 60, 600, 3600, 12 Prelude.* 3600, 72 Prelude.* 3600]  -- updateEvery

initPortfolio = Portfolio
    $ Proxy @"BTCB" := Amount zero
   :& Proxy @"ETH" := Amount zero
   :& Proxy @"WBNB" := Amount one
   :& Empty

main = do
    now <- getCurrentTime
    let t1 = Prelude.negate (100 Prelude.* nominalDay) `addUTCTime` now
        t2 = Prelude.negate (10 Prelude.* nominalDay) `addUTCTime` now
        window = 30 Prelude.* nominalDay
        stride = 7.5 Prelude.* nominalDay

    ps <- runPriceFeed @Assets @Minute prec t1 t2
    --eval <- evaluateOnWindows @Assets prec metrics fees
    --    window stride ps initPortfolio config
    --pPrint eval

    let fitness = instrumentFitness (hourly avgReturn) fees window stride ps
            initPortfolio
    gen <- newStdGen
    tune prec (TrialLimit 32) fitness (runGridRandom gen) grid

-}
