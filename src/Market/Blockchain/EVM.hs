{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}

module Market.Blockchain.EVM where

import Control.Exception hiding (throw)
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
import Polysemy.State
import Prelude hiding (lookup)
import System.IO.Unsafe

data EVM = EVM
    { chainId :: Integer
    , jsonRpcUrl :: String
    , minGasPrice :: Shannon
    , baseAsset :: Asset
    , tokenListName :: String
    }

polygon = EVM
    { chainId = 137
    , jsonRpcUrl = "http://polygon-rpc.com/"
    -- Technically 30, but gas is cheap on Polygon so let's speed things up.
    , minGasPrice = 200
    , baseAsset = Asset "MATIC"
    , tokenListName = "polygon"
    }

polygonTestnet = EVM
    { chainId = 80001
    , jsonRpcUrl = "http://matic-mumbai.chainstacklabs.com"
    , minGasPrice = 40
    , baseAsset = Asset "MATIC"
    , tokenListName = "polygon-testnet"
    }

data WalletEVM = Wallet
    { localKey :: LocalKey
    , myAddress :: Solidity.Address
    }

instance Platform EVM where

    type Effects EVM r = State JsonRpcClient : r
    type Wallet EVM = WalletEVM

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

    fetchPortfolio assets = do
        wallet <- input
        evm <- input
        assetsAndAmounts <- forM assets \asset -> do
            token <- getToken evm asset
            amount
               <- fmap (amountFromSolidity $ decimals token)
                $ retryingCall
                $ web3ToSem
                $ withAccount ()
                $ withParam (to .~ address token)
                $ ERC20.balanceOf
                $ myAddress wallet
            pure (asset, amount)
        pure $ fromList assetsAndAmounts

    runPlatform platform action = do
        manager <- embed $ newManager defaultManagerSettings
        subsume_
            $ evalState (JsonRpcHttpClient manager $ jsonRpcUrl platform)
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

retryingTransaction
    :: Members [Error TransactionError, Error PlatformError, Embed IO] r
    => Sem r a -> Sem r a
retryingTransaction
    -- TODO: Retry with more gas; add an upper limit.
    = Sem.withExponentialBackoff 1 10 \case
        TransactionTimeout -> True
        UnknownTransactionError _ -> True
        _ -> False
    . retryingCall

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
