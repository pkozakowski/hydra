{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Command.Sync
import Control.Logging
import Data.Fixed
import Data.Proxy
import Data.Record.Hom
import Data.Time.Clock
import Market.Feed.MongoDB
import Market.Instruments
import Market.Notebook
import Market.Types
import Numeric.Algebra
import Numeric.Field.Fraction
import Options.Applicative
import System.Random
import Text.Pretty.Simple

data Cmd = Sync SyncOptions

parseCmd :: Parser Cmd
parseCmd = Sync
    <$> hsubparser
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

main :: IO ()
main = run =<< customExecParser (prefs showHelpOnEmpty) opts where
    opts = info (options <**> helper) idm
