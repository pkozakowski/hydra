{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Logging
import Options.Applicative hiding (helper, hsubparser)
import qualified Options.Applicative as Optparse

import Command.Eval
import Command.Run
import Command.Sync
import Command.Tune
import Help

data Cmd
    = Eval EvalOptions
    | Sync SyncOptions
    | Tune TuneOptions
    | Run  RunOptions

parseCmd :: Parser Cmd
parseCmd
    = Eval <$> hsubparser
        ( command "eval"
            $ info evalOptions
            $ progDesc "Evaluate an instrument."
        )
  <|> Run <$> hsubparser
        ( command "run"
            $ info runOptions
            $ progDesc "Run an instrument on the blockchain."
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
                    ( "Verbose mode. By default, only WARNINGs and ERRORs are "
                   <> "shown. -v enables INFO messages, -vv enables DEBUG "
                   <> "messages."
                    )
                )
            )
        )

dispatch :: Options -> IO ()
dispatch opts = withStderrLogging do
    setLogLevel $ toLogLevel $ verbosity opts
    case cmd opts of
        Eval opts' -> eval opts'
        Run  opts' -> run  opts'
        Sync opts' -> sync opts'
        Tune opts' -> tune opts'

main :: IO ()
main = dispatch =<< customExecParser (prefs showHelpOnEmpty) opts where
    opts = info (options <**> helper) idm
