{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Market.Blockchain.EVM.UniswapV2 where

import Control.Logging
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
    }

data SwapConfigUniswapV2 = SwapConfig
    { slippage :: Scalar
    , timeLimit :: NominalDiffTime
    }

quickswap = UniswapV2
    { platform = polygon
    , routerAddress = "0xa5E0829CaCEd8fFDD4De3c43696c57F7D7A678ff"
    }

instance Exchange EVM UniswapV2 where

    type SwapConfig UniswapV2 = SwapConfigUniswapV2

    fetchPrices = undefined

    swap fromAsset toAsset fromAmount = do
        estGasPrice <- web3ToSem $ fromWei <$> Eth.gasPrice
        exchange <- input @UniswapV2
        let evm = platform exchange
            ourGasPrice = max estGasPrice $ minGasPrice evm

        wallet <- input @WalletEVM
        let run
                :: Members [State JsonRpcClient, Error String, Embed IO] r
                => String -> LocalKeyAccount Web3 TxReceipt -> Sem r ()
            run txName action = do
                receipt <- web3ToSem
                    $ withAccount (localKey wallet)
                    $ withParam (gasPrice .~ ourGasPrice) action
                case receiptStatus receipt of
                    Just 1 -> return ()
                    maybeStatus -> throw
                        $ txName ++ " failed with status " ++ show maybeStatus

            runSwap = run "swap" . withParam (to .~ routerAddress exchange)

            wrappedBaseAsset = Asset $ "W" ++ symbol where
                Asset symbol = baseAsset evm

        config <- input @SwapConfigUniswapV2
        prices <- input @Prices
        let toAmount = fromJust $ (prices ! fromAsset) `pi` fromAmount
                `kappa` (prices ! toAsset)
            toAmountMin = (one - slippage config) .* toAmount where
                (-) = (Algebra.-)

        now <- embed getCurrentTime
        let deadline
                = floor $ utcTimeToPOSIXSeconds now + timeLimit config where
                    (+) = (Prelude.+)

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
