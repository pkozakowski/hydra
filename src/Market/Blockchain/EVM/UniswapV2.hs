{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Market.Blockchain.EVM.UniswapV2 where

import Control.Logging
import Control.Monad
import Data.Map.Class
import Data.Maybe
import qualified Data.Solidity.Prim as Solidity
import Data.Text (pack, unpack)
import Data.Time
import Data.Time.Clock.POSIX
import Lens.Micro hiding (to)
import Market
import Market.Blockchain
import Market.Blockchain.EVM
import qualified Market.Blockchain.EVM.ERC20 as ERC20
import Network.Ethereum.Account
import Network.Ethereum.Api.Types
import qualified Network.Ethereum.Api.Eth as Eth
import Network.Ethereum.Contract.TH
import Network.Ethereum.Unit
import Network.JsonRpc.TinyClient
import Network.Web3.Provider
import Numeric.Algebra as Algebra
import Numeric.Field.Fraction
import Numeric.Kappa
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.State
import Prelude hiding (pi)

[abiFrom|abis/uniswap/UniswapV2Router02.json|]

data UniswapV2 = UniswapV2
    { platform :: EVM
    , routerAddress :: Solidity.Address
    , providerFee :: Scalar
    , stablecoin :: Asset
    }

data SwapConfigUniswapV2 = SwapConfig
    { slippage :: Scalar
    , timeLimit :: NominalDiffTime
    }

quickswap = UniswapV2
    { platform = polygon
    , routerAddress = "0xa5E0829CaCEd8fFDD4De3c43696c57F7D7A678ff"
    , providerFee = 3 % 1000
    , stablecoin = Asset "USDC"
    }

instance Exchange EVM UniswapV2 where

    type SwapConfig UniswapV2 = SwapConfigUniswapV2

    fetchPrices assets = do
        exchange <- input
        let evm = platform exchange
            stableAsset = stablecoin exchange
        assetsAndPrices <- forM assets \asset
            -> if asset == stableAsset
                then pure (asset, Price one)
                else do
                    price <- fetchExchangeRate asset stableAsset $ Amount one
                    pure $ (asset, Price price)
        pure $ fromList assetsAndPrices

    swap fromAsset toAsset fromAmount = do
        estGasPrice <- web3ToSem $ fromWei <$> Eth.gasPrice
        exchange <- input
        let evm = platform exchange
            ourGasPrice = max estGasPrice $ minGasPrice evm

        wallet <- input @WalletEVM
        let run
                :: Members [State JsonRpcClient, Error String, Embed IO] r
                => String -> LocalKeyAccount Web3 TxReceipt -> Sem r ()
            run txName action = do
                receipt
                   <- withLocalKeyAccount wallet
                    $ withParam (gasPrice .~ ourGasPrice) action
                case receiptStatus receipt of
                    Just 1 -> return ()
                    maybeStatus -> throw
                        $ txName ++ " failed with status " ++ show maybeStatus

            runSwap = run "swap" . withParam (to .~ routerAddress exchange)

            wrappedBaseAsset = wrapAsset $ baseAsset evm

        config <- input @SwapConfigUniswapV2
        exchangeRate <- fetchExchangeRate fromAsset toAsset fromAmount
        let toAmount = exchangeRate .* fromAmount
            toAmountMin
                = (one - providerFee exchange)
                * (one - slippage config)
               .* toAmount where
                    (-) = (Algebra.-)
                    (*) = (Algebra.*)

        now <- embed getCurrentTime
        let deadline = floor $ utcTimeToPOSIXSeconds now + timeLimit config
                where (+) = (Prelude.+)

        if fromAsset == baseAsset evm then do
            fromToken <- getToken evm wrappedBaseAsset
            toToken <- getToken evm toAsset
            embed $ debug "swapping ETH -> token"
            runSwap
                $ withParam (value .~ amountToEther fromAmount)
                $ swapExactETHForTokens
                    (amountToSolidity (decimals toToken) toAmountMin)
                    [address fromToken, address toToken]
                    (myAddress wallet)
                    deadline
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
                    $ swapExactTokensForETH
                        fromAmountSol
                        (amountToSolidity (decimals toToken) toAmountMin)
                        [address fromToken, address toToken]
                        (myAddress wallet)
                        deadline
            else do
                toToken <- getToken evm toAsset
                embed $ debug "swapping token -> token"
                runSwap
                    $ swapExactTokensForTokens
                        fromAmountSol
                        (amountToSolidity (decimals toToken) toAmountMin)
                        [address fromToken, address toToken]
                        (myAddress wallet)
                        deadline

wrapAsset :: Asset -> Asset
wrapAsset (Asset symbol) = Asset $ "W" ++ symbol

fetchExchangeRate
    :: Members (Input UniswapV2 : Effects EVM) r
    => Asset -> Asset -> Amount -> Sem r Scalar
fetchExchangeRate fromAsset toAsset fromAmount = do
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
        $ withDefaultAccount
        $ withParam (to .~ routerAddress exchange)
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
