{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}

module Market.Blockchain.EVM where

import Crypto.Ethereum
import Control.Monad
import qualified Control.Monad.State.Lazy as MTL
import Data.Aeson
import Data.ByteArray.HexString
import Data.ByteString hiding (pack)
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Solidity.Abi
import qualified Data.Solidity.Prim as Solidity
import qualified Data.Solidity.Prim.Address as Solidity
import Data.Text
import GHC.Generics
import Market
import Market.Blockchain
import Market.Internal.IO
import Network.Ethereum.Account
import Network.Ethereum.Api.Types
import Network.Ethereum.Contract.Method
import Network.Ethereum.Unit
import Network.JsonRpc.TinyClient
import Network.Web3.Provider
import Numeric.Truncatable
import Polysemy
import Polysemy.Error
import Polysemy.State
import Prelude hiding (lookup)
import System.IO.Unsafe

data EVM = EVM
    { chainId :: Integer
    , jsonRpcUrl :: String
    , baseAsset :: Asset
    , tokenListName :: String
    }

polygon = EVM
    { chainId = 137
    , jsonRpcUrl = "https://polygon-rpc.com/"
    , baseAsset = Asset "MATIC"
    , tokenListName = "polygon"
    }

polygonTestnet = EVM
    { chainId = 80001
    , jsonRpcUrl = "https://matic-mumbai.chainstacklabs.com"
    , baseAsset = Asset "MATIC"
    , tokenListName = "polygon-testnet"
    }

data WalletEVM = WalletEVM 
    { localKey :: LocalKey
    , myAddress :: Solidity.Address
    }

instance Platform EVM where

    type Effects EVM = [State JsonRpcClient, Embed IO]
    type Wallet EVM = WalletEVM

    loadWallet platform bs = do
        hex <- fromEither $ hexString bs
        let key = importKey $ toBytes @ByteString hex
        return WalletEVM
            { localKey = LocalKey key $ chainId platform
            , myAddress = Solidity.fromPubKey $ derivePubKey key
            }

web3ToSem
    :: Members [State JsonRpcClient, Embed IO] r
    => Web3 a -> Sem r a
web3ToSem action = do
    state <- get
    (result, state')
       <- embed
        $ flip MTL.runStateT state
        $ unWeb3 action
    put state'
    return result

amountToEther :: Amount -> Ether
amountToEther (Amount amount) = fractionToFractional amount

amountToSolidity :: Integer -> Amount -> Solidity.UIntN 256
amountToSolidity decimals (Amount amount)
    = floor $ fractionToFractional amount * fromInteger (10 ^ decimals)

data Token = Token
    { symbol :: Asset
    , address :: Solidity.Address
    , decimals :: Integer
    } deriving (FromJSON, Generic)

tokenListCache :: IORef (Map String (Map Asset Token))
tokenListCache = unsafePerformIO $ newIORef Map.empty
{-# NOINLINE tokenListCache #-}

getToken
    :: Members [Error String, Embed IO] r
    => EVM -> Asset -> Sem r Token
getToken evm asset = do
    tokenList <- embed $ cacheF
        tokenListCache
        readTokenList
        (\name -> "token list " <> pack name)
        (tokenListName evm)
    case Map.lookup asset tokenList of
        Just token -> return token
        Nothing -> throw
            $ "token " ++ show asset
           ++ " not found on the list " ++ show (tokenListName evm)
    where
        readTokenList name = do
            eitherTokens <- eitherDecodeFileStrict
                $ "tokens/" ++ name ++ ".json"
            case eitherTokens of
                Left error -> fail
                    $ "error when decoding token list " ++ name ++ ": " ++ error
                Right tokens
                    -> return $ Map.fromList $ addKey <$> tokens where
                        addKey token = (symbol token, token)
