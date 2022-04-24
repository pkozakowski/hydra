{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Market.Blockchain.EVM.UniswapV2 where

import Control.Exception hiding (throw)
import Control.Logging
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
import qualified Data.Aeson as Aeson
import Data.ByteArray.HexString
import Data.List
import Data.Map.Class
import Data.Maybe
import qualified Data.Solidity.Prim as Solidity
import Data.Text (pack, unpack)
import Data.Time
import Data.Time.Clock.POSIX
import Dhall (FromDhall)
import GHC.Generics (Generic)
import Lens.Micro hiding (to)
import Market
import Market.Blockchain
import Market.Blockchain.EVM
import qualified Market.Blockchain.EVM.ERC20 as ERC20
import Market.Internal.Sem
import Network.Ethereum.Account as EthAcc
import Network.Ethereum.Api.Types hiding (Block, Latest)
import qualified Network.Ethereum.Api.Types as Eth hiding (gasPrice)
import qualified Network.Ethereum.Api.Eth as Eth
import Network.Ethereum.Contract.TH
import Network.Ethereum.Unit
import Network.JsonRpc.TinyClient
import Network.Web3.Provider
import Numeric.Algebra as Algebra
import Numeric.Field.Fraction
import Numeric.Kappa
import Polysemy
import Polysemy.Error hiding (fromException)
import Polysemy.Input
import Polysemy.Reader
import Polysemy.State
import Prelude hiding (pi)

[abiFrom|abis/uniswap/UniswapV2Router02.json|]

data UniswapV2 = UniswapV2
    { platform :: EVM
    , routerAddress :: Solidity.Address
    , providerFee :: Scalar
    , stablecoin :: Asset
    } deriving (Generic, FromDhall)

data SwapConfigUniswapV2 = SwapConfig
    { slippage :: Scalar
    , timeLimit :: NominalDiffTime
    } deriving (Generic, FromDhall)

instance Exchange EVM UniswapV2 where

    type SwapConfig UniswapV2 = SwapConfigUniswapV2

    fetchPricesAt block assets = do
        exchange <- input
        let evm = platform exchange
            stableAsset = stablecoin exchange
        assetsAndPrices <- forM assets \asset
            -> if asset == stableAsset
                then pure (asset, Price one)
                else do
                    price <- fetchExchangeRateAt
                            (const @_ @SomeException $ pure ())
                            block asset stableAsset
                        $ Amount one
                    pure $ (asset, Price price)
        pure $ fromList assetsAndPrices

    estimateFees = do
        exchange <- input @UniswapV2
        return $ Fees
            { variable = providerFee exchange
            -- TODO: estimate based on the contracts, then update the estimate
            -- based on the actual transactions
            , fixed = Nothing
            }

    swap fromAsset toAsset fromAmount = retryingSwap do
        wallet <- input @WalletEVM
        config <- input @SwapConfigUniswapV2
        exchange <- input
        let evm = platform exchange
            run
                :: forall r
                 . Members (SwapEffects EVM UniswapV2) r
                => String
                -> LocalKeyAccount Web3 (Either HexString TxReceipt)
                -> Sem r ()
            run txName action = retryingTransaction do
                estGasPrice <- retryingCall $ web3ToSem $ fromWei <$> Eth.gasPrice
                ourGasPrice <- max estGasPrice <$> askMinGasPrice
                maxGasPrice <- maxGasPrice <$> ask
                when (ourGasPrice Prelude.> maxGasPrice)
                    $ throw GasTooExpensive

                now <- embed $ getCurrentTime
                let deadline = timeLimit config `addUTCTime` now
                    timeoutUs = floor $ timeLimit config Prelude.* 1e6
                receiptOrHash
                   <- web3ToSem' handleUniswapException
                    $ withAccount (localKey wallet)
                    $ withParam (gasPrice .~ ourGasPrice)
                    $ withParam (timeout .~ Just timeoutUs)
                    $ action
                case receiptOrHash of
                    Right receipt -> case receiptStatus receipt of
                        Just 1 -> return ()
                        maybeStatus -> do
                            -- Sometimes an expired transaction reverts without
                            -- an error message before the off-chain timeout
                            -- triggers.
                            now' <- embed $ getCurrentTime
                            if now' Prelude.> deadline
                                then throw RetryableTransactionTimeout
                                else throw @TransactionError
                                    $ UnknownTransactionError
                                    $ txName ++ " failed with status "
                                   ++ show maybeStatus
                    Left _ -> throw RetryableTransactionTimeout

            runSwap = run "swap" . withParam (to .~ routerAddress exchange)

            wrappedBaseAsset = wrapAsset $ baseAsset evm

        exchangeRate <- fetchExchangeRateAt
            handleUniswapException Latest fromAsset toAsset fromAmount
        let toAmount = exchangeRate .* fromAmount
            toAmountMin
                = (one - providerFee exchange)
                * (one - slippage config)
               .* toAmount where
                    (-) = (Algebra.-)
                    (*) = (Algebra.*)

        let withDeadline action = do
                now <- lift $ liftIO getCurrentTime
                let deadline = floor $ utcTimeToPOSIXSeconds now + timeLimit config
                        where (+) = (Prelude.+)
                action deadline

        if fromAsset == baseAsset evm then do
            fromToken <- getToken evm wrappedBaseAsset
            toToken <- getToken evm toAsset
            embed $ debug "swapping ETH -> token"
            runSwap
                $ withParam (value .~ amountToEther fromAmount)
                $ withDeadline
                $ swapExactETHForTokens
                    (amountToSolidity (decimals toToken) toAmountMin)
                    [address fromToken, address toToken]
                    (myAddress wallet)
        else do
            fromToken <- getToken evm fromAsset
            let fromAmountSol = amountToSolidity (decimals fromToken) fromAmount
            embed $ debug $ "approving " <> pack (show fromAsset)
            run "approval"
                $ withParam (to .~ address fromToken)
                $ ERC20.approve (routerAddress exchange) fromAmountSol

            if toAsset == baseAsset evm then do
                toToken <- getToken evm wrappedBaseAsset
                embed $ debug "swapping token -> ETH"
                runSwap
                    $ withDeadline
                    $ swapExactTokensForETH
                        fromAmountSol
                        (amountToSolidity (decimals toToken) toAmountMin)
                        [address fromToken, address toToken]
                        (myAddress wallet)
            else do
                toToken <- getToken evm toAsset
                embed $ debug "swapping token -> token"
                runSwap
                    $ withDeadline
                    $ swapExactTokensForTokens
                        fromAmountSol
                        (amountToSolidity (decimals toToken) toAmountMin)
                        [address fromToken, address toToken]
                        (myAddress wallet)
        where
            retryingSwap = withExponentialBackoff 1 10 \case
                PriceSlipped -> True

wrapAsset :: Asset -> Asset
wrapAsset (Asset symbol) = Asset $ "W" ++ symbol

fetchExchangeRateAt
    :: Members (Input UniswapV2 : PlatformEffects EVM) r
    => (SomeException -> Sem r ())
    -> Block
    -> Asset
    -> Asset
    -> Amount
    -> Sem r Scalar
fetchExchangeRateAt handler block fromAsset toAsset fromAmount = retryingCall do
    embed
        $ debug
        $ "fetching exchange rate: " <> pack (show fromAsset)
       <> " -> " <> pack (show toAsset)
    exchange <- input
    let evm = platform exchange
    fromToken <- getTokenWithWrap evm fromAsset
    toToken <- getTokenWithWrap evm toAsset
    Amount priceWithFee
       <- fmap (amountFromSolidity (decimals toToken) . last)
        $ web3ToSem' handler
        $ withAccount ()
        $ withParam (to .~ routerAddress exchange)
        $ withParam (EthAcc.block .~ blockToEth block)
        $ getAmountsOut
            (amountToSolidity (decimals fromToken) fromAmount)
            [address fromToken, address toToken]
    pure $ priceWithFee / (one - providerFee exchange)
    where
        getTokenWithWrap evm asset
            = getToken evm
            $ if asset == baseAsset evm
                then wrapAsset asset
                else asset
        (-) = (Algebra.-)
        (/) = (Algebra./)

handleUniswapException
    :: Members [Error SwapError, Error TransactionError, Embed IO] r
    => SomeException -> Sem r ()
handleUniswapException exc = do
    case maybeJsonRpcExc of
        Just (CallException rpcError)
         -> if rpcError `has` "INSUFFICIENT_INPUT_AMOUNT"
                then throw PriceSlipped
            else if rpcError `has` "EXPIRED"
                then throw RetryableTransactionTimeout
            else if rpcError `has` outOfGas
                then throw OutOfGas
                else unknown
        Just _ -> unknown
        Nothing -> unknown
    where
        maybeJsonRpcExc :: Maybe JsonRpcException
        maybeJsonRpcExc = fromException exc
        has rpcError msg
            = msg `isInfixOf` unpack (errMessage rpcError)
        outOfGas = "gas required exceeds allowance"
        unknown = throw $ UnknownTransactionError $ displayException exc
