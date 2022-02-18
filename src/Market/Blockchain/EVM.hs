{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}

module Market.Blockchain.EVM where

import Control.Exception hiding (throw)
import Control.Logging
import Control.Monad
import qualified Control.Monad.State.Lazy as MTL
import Crypto.Ethereum
import Data.Aeson
import Data.ByteArray.HexString
import Data.ByteString hiding (pack)
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Class
import Data.Solidity.Abi
import qualified Data.Solidity.Prim as Solidity
import qualified Data.Solidity.Prim.Address as Solidity
import Data.Text
import GHC.Generics hiding (to)
import Lens.Micro hiding (to)
import Market
import Market.Blockchain
import qualified Market.Blockchain.EVM.ERC20 as ERC20
import Market.Internal.IO
import Market.Internal.Sem as Sem
import Network.Ethereum.Account
import Network.Ethereum.Api.Types hiding (TransactionTimeout)
import Network.Ethereum.Contract.Method
import Network.Ethereum.Unit
import Network.HTTP.Client
import Network.JsonRpc.TinyClient
import Network.Web3.Provider
import Numeric.Field.Fraction
import Numeric.Truncatable
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Reader
import Polysemy.State
import Prelude hiding (log, lookup)
import System.IO.Unsafe

data EVM = EVM
    { chainId :: Integer
    , jsonRpcUrl :: String
    , platformMinGasPrice :: Shannon
    , baseAsset :: Asset
    , tokenListName :: String
    }

polygon = EVM
    { chainId = 137
    , jsonRpcUrl = "http://polygon-rpc.com/"
    , platformMinGasPrice = 30
    , baseAsset = Asset "MATIC"
    , tokenListName = "polygon"
    }

polygonTestnet = EVM
    { chainId = 80001
    , jsonRpcUrl = "http://matic-mumbai.chainstacklabs.com"
    , platformMinGasPrice = 30
    , baseAsset = Asset "MATIC"
    , tokenListName = "polygon-testnet"
    }

data WalletEVM = Wallet
    { localKey :: LocalKey
    , myAddress :: Solidity.Address
    }

data ConfigEVM = Config
    { minGasPrice :: Maybe Shannon
    , maxGasPrice :: Shannon
    }

instance Platform EVM where

    type Effects EVM = '[State JsonRpcClient]
    type Wallet EVM = WalletEVM
    type PlatformConfig EVM = ConfigEVM

    loadWallet bs = do
        platform <- input
        case hexString bs of
            Left error -> throw $ CantLoadWallet error
            Right hex -> do
                let key = importKey $ toBytes @ByteString hex
                return Wallet
                    { localKey = LocalKey key $ chainId platform
                    , myAddress = Solidity.fromPubKey $ derivePubKey key
                    }

    fetchBalance asset = do
        wallet <- input
        evm <- input
        token <- getToken evm asset
        fmap (amountFromSolidity $ decimals token)
            $ retryingCall
            $ web3ToSem
            $ withAccount ()
            $ withParam (to .~ address token)
            $ ERC20.balanceOf
            $ myAddress wallet

    runPlatform platform config action = do
        manager <- embed $ newManager defaultManagerSettings
        subsume_
            $ evalState (JsonRpcHttpClient manager $ jsonRpcUrl platform)
            $ runReader config
            $ runInputConst platform
            $ action

retryingCall
    :: Members [Error PlatformError, Embed IO] r
    => Sem r a -> Sem r a
retryingCall = Sem.withExponentialBackoff 0.1 10 \case
    TransportError _ -> True
    CallTimeout -> True
    UnknownPlatformError _ -> True
    _ -> False

askMinGasPrice :: Members [Input EVM, Reader ConfigEVM] r => Sem r Shannon
askMinGasPrice = do
    platformMin <- platformMinGasPrice <$> input @EVM
    maybe platformMin (max platformMin) . minGasPrice <$> ask @ConfigEVM

retryingTransaction
    :: forall r a
     . Members (TransactionEffects EVM) r
    => Sem r a -> Sem r a
retryingTransaction action = do
    platformMin <- platformMinGasPrice <$> input
    let increaseGas
            = local @ConfigEVM
                ( \config -> config
                    { minGasPrice = Just $ dbl $ calcMin (minGasPrice config) }
                ) where
                    dbl x = x + x  -- * 2 increases the price * 2e9 (?!)
                    calcMin = \case
                        Just configMin -> max platformMin configMin
                        Nothing -> platformMin
    origMin <- askMinGasPrice
    Sem.withExponentialBackoff' 1 10
        ( \case
            TransactionTimeout True -> Just . increaseGas
            TransactionTimeout False -> const Nothing
            GasTooExpensive -> Just
            UnknownTransactionError _ -> Just
            _ -> const Nothing
        ) do
            minGasPrice <- askMinGasPrice
            maxGasPrice <- maxGasPrice <$> ask
            when (minGasPrice > maxGasPrice)
                -- Don't retry.
                $ throw $ TransactionTimeout False
            when (minGasPrice > origMin)
                $ embed
                $ debug
                $ "min gas price increased to "
               <> pack (show minGasPrice)
            action

web3ToSem
    :: Members (PlatformEffects EVM) r
    => Web3 a -> Sem r a
web3ToSem = web3ToSem' $ const @_ @SomeException $ pure ()

web3ToSem'
    :: forall e r a
     . (Exception e, Members (PlatformEffects EVM) r)
    => (e -> Sem r ()) -> Web3 a -> Sem r a
web3ToSem' handler action = do
    state <- get
    (result, state')
       <- ioToSem
        $ flip MTL.runStateT state
        $ unWeb3 action
    put @JsonRpcClient state'
    return result
    where
        ioToSem
            :: Members (PlatformEffects EVM) r
            => IO b -> Sem r b
        ioToSem action = do
            resultOrError
               <- embed
                $ Control.Exception.try @e
                $ Control.Exception.try @HttpException
                $ action
            case resultOrError of
                Right (Right result) -> pure result
                Right (Left exc) -> throw $ TransportError $ show exc
                Left exc -> do
                    -- If the handler triggers (throws), the next line won't
                    -- execute.
                    handler exc
                    throw $ UnknownPlatformError $ displayException exc

amountToEther :: Amount -> Ether
amountToEther (Amount amount) = fractionToFractional amount

amountToSolidity :: Integer -> Amount -> Solidity.UIntN 256
amountToSolidity decimals (Amount amount)
    = floor $ fractionToFractional amount * fromInteger (10 ^ decimals)

amountFromSolidity :: Integer -> Solidity.UIntN 256 -> Amount
amountFromSolidity decimals amount
    = Amount $ fromIntegral amount % 10 ^ decimals

data Token = Token
    { symbol :: Asset
    , address :: Solidity.Address
    , decimals :: Integer
    } deriving (FromJSON, Generic)

tokenListCache :: IORef (Map String (Map Asset Token))
tokenListCache = unsafePerformIO $ newIORef Map.empty
{-# NOINLINE tokenListCache #-}

getToken
    :: forall r
     . Members [Error PlatformError, Embed IO] r
    => EVM -> Asset -> Sem r Token
getToken evm asset = do
    tokenList <- cacheF
        tokenListCache
        readTokenList
        (\name -> "token list " <> pack name)
        (tokenListName evm)
    case Map.lookup asset tokenList of
        Just token -> return token
        Nothing -> throw $ NoSuchAsset asset
    where
        readTokenList :: String -> Sem r (Map Asset Token)
        readTokenList name = do
            eitherTokens
               <- embed
                $ eitherDecodeFileStrict
                $ "tokens/" ++ name ++ ".json"
            case eitherTokens of
                Left error -> throw
                    $ LocalError
                    $ "when decoding token list " ++ name ++ ": " ++ error
                Right tokens
                    -> return $ Map.fromList $ addKey <$> tokens where
                        addKey token = (symbol token, token)
