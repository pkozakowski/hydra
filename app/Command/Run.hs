module Command.Run where

import Control.Logging
import Data.List
import Data.Map.Class ((!))
import Data.String
import Data.Text (Text, pack, strip, unpack)
import qualified Dhall
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
    , config :: String
    }

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
        ( metavar "CONFIG"
       <> help "Instrument configuration script."
        )

-- TODO: pass through options
platformConfig :: ConfigEVM
platformConfig = Config
    { minGasPrice = Nothing
    , maxGasPrice = 1000
    }

-- TODO: pass through options
swapConfig = SwapConfig
    { slippage = 1 % 100
    , timeLimit = fromInteger 15
    }

-- TODO: infer; in the future, adjust in runtime
fees = Fees
    { variable = 3 % 1000
    , fixed = Nothing
    }

run :: RunOptions -> IO ()
run options = do
    wallet
       <- fmap (fromString . unpack . strip . pack)
        $ readFile
        $ wallet options

    config
        :: SomeInstrumentConfig
        <- Dhall.input Dhall.auto
         $ "./" <> pack (config options) <> " ./dhall/Market/Instrument/Type"

    withStderrLogging
        $ semToIO
        $ mapError @PlatformError show
        $ mapError @TransactionError show
        $ mapError @SwapError show
        $ mapError @MarketError show
        $ runPlatform polygon platformConfig do
            wallet <- loadWallet @EVM wallet
            runExchange quickswap swapConfig wallet
                $ runTimeIO
                -- -- TODO: add baseAsset to the Platform API
                $ runInputConst (nub $ baseAsset polygon : managedAssets config)
                $ runInputPortfolioBlockchain @EVM
                $ runInputPricesBlockchain @EVM @UniswapV2
                $ runMarketBlockchain @EVM @UniswapV2
                $ runInstrument config 
                $ runInputConst fees
                $ do
                    prices <- input @Prices
                    portfolio <- input @Portfolio
                    -- TODO: make it continue when a transaction fails
                    execute

    pure ()
