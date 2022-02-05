{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}

module Market.Blockchain where

import Data.ByteString
import Data.Constraint
import Data.Fixed
import Market
import Polysemy
import Polysemy.Error
import Polysemy.Input

class Platform p where

    type Effects p :: EffectRow
    type Wallet p :: *

    loadWallet
        :: Members (Input p : Effects p) r
        => ByteString -> Sem r (Wallet p)

    fetchPortfolio
        :: Members (Input p : Input (Wallet p) : Error String : Effects p) r
        => [Asset] -> Sem r Portfolio

    runPlatform
        :: Members [Error String, Embed IO] r
        => p -> Sem (Input p : Effects p) a -> Sem r a

class Platform p => Exchange p e | e -> p where

    type SwapConfig e :: *

    fetchPrices
        :: Members (Input e : Input (Wallet p) : Error String : Effects p) r
        => [Asset] -> Sem r Prices
    
    swap
        :: Members
            ( Input e
            : Input (SwapConfig e)
            : Input (Wallet p)
            : Error String
            : Effects p
            ) r
        => Asset -> Asset -> Amount -> Sem r ()

runExchange
    :: Exchange p e
    => e
    -> SwapConfig e
    -> Wallet p
    -> Sem
        ( Input e
        : Input (SwapConfig e)
        : Input (Wallet p)
        : r
        ) a
    -> Sem r a
runExchange exchange swapConfig wallet
    = runInputConst wallet
    . runInputConst swapConfig
    . runInputConst exchange
