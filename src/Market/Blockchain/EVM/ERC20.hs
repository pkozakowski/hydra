{-# LANGUAGE QuasiQuotes #-}

module Market.Blockchain.EVM.ERC20 where

import Network.Ethereum.Contract.TH

[abiFrom|abis/ERC20.json|]
