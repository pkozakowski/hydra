{-# LANGUAGE QuasiQuotes #-}

module Market.Blockchain.EVM.UniswapV2 where

import Control.Logging
import Data.Map.Class
import qualified Data.Solidity.Prim as Solidity
import Data.Time
import Data.Time.Clock.POSIX
import Lens.Micro hiding (to)
import Market
import Market.Blockchain
import Market.Blockchain.EVM
import Network.Ethereum.Account
import Network.Ethereum.Contract.TH
import Network.Ethereum.Unit
import Network.JsonRpc.TinyClient
import Network.Web3.Provider
import Numeric.Algebra ((.*))
import Numeric.Field.Fraction
import Numeric.Kappa
import Polysemy
import Polysemy.Input
import Polysemy.State
import Prelude hiding (pi)

[abiFrom|abis/uniswap/UniswapV2Router02.json|]

data UniswapV2 = UniswapV2
    { platform :: EVM
    , routerAddress :: Solidity.Address
    }

quickswap = UniswapV2
    { platform = polygon
    , routerAddress = "0xa5E0829CaCEd8fFDD4De3c43696c57F7D7A678ff"
    }

instance Exchange EVM UniswapV2 where

    fetchPrices = undefined

    swap exchange wallet fromAsset toAsset fromAmount = do
        prices <- input @Prices
        let toAmount = (prices ! fromAsset) `pi` fromAmount
                `kappa` (prices ! toAsset)
            -- 1% slippage
            toAmountMin = (99 % 100 :: Fraction Integer) .* toAmountMin

        now <- embed getCurrentTime
        -- 1 minute deadline
        let deadline = floor $ utcTimeToPOSIXSeconds now + 60

        if fromAsset == baseAsset evm then do
            fromToken <- getToken evm wrappedBaseAsset
            toToken <- getToken evm toAsset
            embed $ debug "swapping ETH -> token"
            _ <- run $ swapExactETHForTokens
                    (amountToSolidity (decimals toToken) toAmountMin)
                    [address fromToken, address toToken]
                    (myAddress wallet)
                    deadline
            return ()
        else if toAsset == baseAsset evm then do
            fromToken <- getToken evm fromAsset
            toToken <- getToken evm wrappedBaseAsset
            embed $ debug "swapping token -> ETH"
            _ <- run $ swapExactTokensForETH
                    (amountToSolidity (decimals fromToken) fromAmount)
                    (amountToSolidity (decimals toToken) toAmountMin)
                    [address fromToken, address toToken]
                    (myAddress wallet)
                    deadline
            return ()
        else do
            fromToken <- getToken evm fromAsset
            toToken <- getToken evm toAsset
            embed $ debug "swapping token -> token"
            _ <- run $ swapExactTokensForTokens
                    (amountToSolidity (decimals fromToken) fromAmount)
                    (amountToSolidity (decimals toToken) toAmountMin)
                    [address fromToken, address toToken]
                    (myAddress wallet)
                    deadline
            return ()
        -- TODO: wait for transactions
        where
            evm = platform exchange
            wrappedBaseAsset = Asset $ "W" ++ symbol where
                Asset symbol = baseAsset evm

            run
                :: Members [State JsonRpcClient, Embed IO] r
                => LocalKeyAccount Web3 a -> Sem r a
            run
                = web3ToSem
                . withAccount (localKey wallet)
                . withParam (to .~ routerAddress exchange)
                . if fromAsset == baseAsset evm
                    then id
                    else withParam $ value .~ amountToEther fromAmount
