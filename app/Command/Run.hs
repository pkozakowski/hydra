{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module Command.Run where

import Control.Logging
import Data.List
import Data.Map.Class ((!))
import Data.String
import Data.Text (Text, pack, strip, unpack)
import Dhall (FromDhall)
import qualified Dhall as Dh
import GHC.Generics
import Market
import Market.Blockchain
import Market.Blockchain.EVM
import Market.Blockchain.EVM.UniswapV2
import Market.Instrument
import Market.Internal.IO
import Market.Time
import Numeric.Field.Fraction
import Options.Applicative
import Polysemy
import Polysemy.Error
import Polysemy.Input

data RunOptions = RunOptions
    { wallet :: String
    , blockchainConfig :: String
    , config :: String
    }

data BlockchainConfig = BlockchainConfig
    { exchange :: UniswapV2
    , platformConfig :: ConfigEVM
    , swapConfig :: SwapConfigUniswapV2
    } deriving (Generic, FromDhall)

runOptions :: Parser RunOptions
runOptions = RunOptions
    <$> argument str
        ( metavar "WALLET"
       <> help
            ( "The wallet file. It should contain the private key as a "
           <> "hexadecimal string."
            )
        )
    <*> argument str
        ( metavar "BLOCKCHAIN_CONFIG"
       <> help "Blockchain configuration script."
        )
    <*> argument str
        ( metavar "INSTRUMENT_CONFIG"
       <> help "Instrument configuration script."
        )

run :: RunOptions -> IO ()
run options = do
    wallet
       <- fmap (fromString . unpack . strip . pack)
        $ readFile
        $ wallet options

    BlockchainConfig {..}
       <- Dh.input Dh.auto
        $ "./" <> pack (blockchainConfig options)

    config
        :: SomeInstrumentConfig
        <- Dh.input Dh.auto
         $ "./" <> pack (config options) <> " ./dhall/Market/Instrument/Type"

    withStderrLogging
        $ semToIO
        $ mapError @PlatformError show
        $ mapError @TransactionError show
        $ mapError @SwapError show
        $ mapError @MarketError show
        $ runPlatform (platform exchange) platformConfig do
            wallet <- loadWallet @EVM wallet
            runExchange exchange swapConfig wallet do
                fees <- estimateFees @EVM @UniswapV2
                runTimeIO
                    $ runInputConst
                        (nub $ managedAssets config ++ feeAssets fees)
                    $ runInputPortfolioBlockchain @EVM
                    $ runInputPricesBlockchain @EVM @UniswapV2
                    $ runMarketBlockchain @EVM @UniswapV2
                    $ runInstrument config
                    $ runInputConst fees
                    $ do
                        prices <- input @Prices
                        portfolio <- input @Portfolio
                        -- TODO: parallel trades, logging
                        execute

    pure ()
