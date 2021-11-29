{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Logging
import Options.Applicative hiding (helper, hsubparser)

import Command.Eval
import Command.Sync
import Command.Tune
import Help

data Cmd
    = Eval EvalOptions
    | Sync SyncOptions
    | Tune TuneOptions

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
  <|> Tune <$> hsubparser
        ( command "tune"
            $ info tuneOptions
            $ progDesc "Tune an instrument."
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
        Eval opts' -> eval opts'
        Sync opts' -> sync opts'
        Tune opts' -> tune opts'

main :: IO ()
main = run =<< customExecParser (prefs showHelpOnEmpty) opts where
    opts = info (options <**> helper) idm
