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
import Market.Strategy
import Market.Time
import Numeric.Field.Fraction
import Options.Applicative
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Logging

data RunOptions = RunOptions
  { brokerConfig :: String
  , strategyConfig :: String
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
      ( metavar "Strategy_CONFIG"
          <> help "Strategy configuration script."
      )

run :: Members [Error String, Logging, Final IO] r => RunOptions -> Sem r ()
run options = embedToFinal do
  broker
    :: DummyBroker <-
    embed
      $ Dh.input
        Dh.auto
      $ "./" <> pack (brokerConfig options)

  strategy
    :: SomeStrategyConfig <-
    embed $
      Dh.input Dh.auto $
        "./" <> pack (strategyConfig options) <> " ./dhall/Market/Strategy/Type"

  mapError @BrokerError show $
    mapError @MarketError show $
      runBroker broker do
        fees <- estimateFees
        runTimeIO
          $ runInputConst
            (nub $ managedAssets strategy ++ feeAssets fees)
          $ runInputPortfolioBroker
          $ runInputPricesBroker
          $ runMarketBroker @DummyBroker
          $ runStrategy strategy
          $ runInputConst fees
          $ do
            prices <- input @Prices
            portfolio <- input @Portfolio
            -- TODO: parallel trades, logging
            execute

  pure ()
