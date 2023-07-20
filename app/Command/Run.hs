module Command.Run where

import Data.List
import Data.Map.Class ((!))
import Data.String
import Data.Text (Text, pack, strip, unpack)
import Dhall (FromDhall)
import Dhall qualified as Dh
import GHC.Generics
import Market
import Market.Broker
import Market.Instrument
import Market.Log (Log)
import Market.Time
import Numeric.Field.Fraction
import Options.Applicative
import Polysemy
import Polysemy.Error
import Polysemy.Input

data RunOptions = RunOptions
  { brokerConfig :: String
  , instrumentConfig :: String
  }

runOptions :: Parser RunOptions
runOptions =
  RunOptions
    <$> argument
      str
      ( metavar "BROKER_CONFIG"
          <> help "Broker configuration script."
      )
    <*> argument
      str
      ( metavar "INSTRUMENT_CONFIG"
          <> help "Instrument configuration script."
      )

run :: Members [Error String, Log, Embed IO] r => RunOptions -> Sem r ()
run options = do
  broker
    :: DummyBroker <-
    embed
      $ Dh.input
        Dh.auto
      $ "./" <> pack (brokerConfig options)

  instrument
    :: SomeInstrumentConfig <-
    embed $
      Dh.input Dh.auto $
        "./" <> pack (instrumentConfig options) <> " ./dhall/Market/Instrument/Type"

  mapError @BrokerError show $
    mapError @MarketError show $
      runBroker broker do
        fees <- estimateFees
        runTimeIO
          $ runInputConst
            (nub $ managedAssets instrument ++ feeAssets fees)
          $ runInputPortfolioBroker
          $ runInputPricesBroker
          $ runMarketBroker @DummyBroker
          $ runInstrument instrument
          $ runInputConst fees
          $ do
            prices <- input @Prices
            portfolio <- input @Portfolio
            -- TODO: parallel trades, logging
            execute

  pure ()
