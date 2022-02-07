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

data PlatformError
    = CantLoadWallet String
    | NoSuchAsset Asset
    | TransportError String
    | LocalError String
    | CallTimeout
    | UnknownPlatformError String
    deriving Show

data TransactionError
    = OutOfGas
    | TransactionTimeout
    | UnknownTransactionError String
    deriving Show

type PlatformEffects p = Effects p [Error PlatformError, Embed IO]

type TransactionEffects p = Effects p
    [ Error TransactionError
    , Error PlatformError
    , Embed IO
    ]

class Platform p where

    type Effects p (r :: EffectRow) :: EffectRow
    type Wallet p :: *

    loadWallet
        :: Members (Input p : PlatformEffects p) r
        => ByteString -> Sem r (Wallet p)

    fetchPortfolio
        :: Members
            ( Input p
            : Input (Wallet p)
            : PlatformEffects p
            ) r
        => [Asset] -> Sem r Portfolio

    runPlatform
        :: Member (Embed IO) r
        => p -> Sem (Input p : Effects p r) a -> Sem r a

data SwapError
    = PriceSlipped
    deriving Show

type SwapEffects p = Effects p
    [ Error SwapError
    , Error TransactionError
    , Error PlatformError
    , Embed IO
    ]

class Platform p => Exchange p e | e -> p where

    type SwapConfig e :: *

    fetchPrices
        :: Members
            ( Input e
            : Input (Wallet p)
            : PlatformEffects p
            ) r
        => [Asset] -> Sem r Prices
    
    swap
        :: Members
            ( Input e
            : Input (SwapConfig e)
            : Input (Wallet p)
            : SwapEffects p
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
