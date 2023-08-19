{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Command.Eval
import Command.Run
import Command.Sync
import Command.Tune
import Df1 qualified
import Help
import Options.Applicative hiding (helper, hsubparser)
import Options.Applicative qualified as Optparse
import Polysemy.Error (errorToIOFinal)
import Polysemy.Final
import Polysemy.Logging

data Cmd
  = Eval EvalOptions
  | Sync SyncOptions
  | Tune TuneOptions
  | Run RunOptions

parseCmd :: Parser Cmd
parseCmd =
  --  Eval
  --    <$> hsubparser
  --      ( command "eval" $
  --          info evalOptions $
  --            progDesc "Evaluate an instrument."
  --      )
  --    <|> Run
  Run
    <$> hsubparser
      ( command "run" $
          Optparse.info runOptions $
            progDesc "Run an instrument on the blockchain."
      )
    <|> Sync
      <$> hsubparser
        ( command "sync" $
            Optparse.info syncOptions $
              progDesc "Synchronize price data."
        )

--  <|> Tune <$> hsubparser
--        ( command "tune"
--            $ info tuneOptions
--            $ progDesc "Tune an instrument."
--        )
--
data Verbosity = Warning | Info | Debug
  deriving (Enum)

toLogLevel :: Verbosity -> Df1.Level
toLogLevel = \case
  Warning -> Df1.Warning
  Info -> Df1.Info
  Debug -> Df1.Debug

data Options = Options
  { cmd :: Cmd
  , verbosity :: Verbosity
  }

options :: Parser Options
options =
  Options
    <$> parseCmd
    <*> ( toEnum . length
            <$> many
              ( flag'
                  ()
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
dispatch Options {..} = runFinal do
  unitOrError <- errorToIOFinal $ runLogging (toLogLevel verbosity) do
    case cmd of
      -- Eval opts' -> eval opts'
      Run opts' -> run opts'
      Sync opts' -> sync opts'
  case unitOrError of
    Right () -> pure ()
    Left err -> Prelude.error err

-- Tune opts' -> tune opts'

main :: IO ()
main = dispatch =<< customExecParser (prefs showHelpOnEmpty) opts
  where
    opts = Optparse.info (options <**> helper) idm
