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
import Polysemy.Reader

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
    | GasTooExpensive
    | TransactionTimeout Bool -- Retryable?
    | UnknownTransactionError String
    deriving Show

transactionTimeout :: TransactionError
transactionTimeout = TransactionTimeout True

type PlatformEffects p = Effects p
    [ Input p
    , Reader (PlatformConfig p)
    , Error PlatformError
    , Embed IO
    ]

type TransactionEffects p = Effects p
    [ Input p
    , Reader (PlatformConfig p)
    , Error TransactionError
    , Error PlatformError
    , Embed IO
    ]

class Platform p where

    type Effects p (r :: EffectRow) :: EffectRow
    type Wallet p :: *
    type PlatformConfig p :: *

    loadWallet
        :: Members (PlatformEffects p) r
        => ByteString -> Sem r (Wallet p)

    fetchPortfolio
        :: Members
            ( Input (Wallet p)
            : PlatformEffects p
            ) r
        => [Asset] -> Sem r Portfolio

    runPlatform
        :: Member (Embed IO) r
        => p
        -> PlatformConfig p
        -> Sem
            ( Input p
            : Reader (PlatformConfig p)
            : Effects p r
            ) a
        -> Sem r a

data SwapError
    = PriceSlipped
    deriving Show

type SwapEffects p e = Effects p
    [ Input p
    , Input (Wallet p)
    , Reader (PlatformConfig p)
    , Input e
    , Input (SwapConfig e)
    , Error SwapError
    , Error TransactionError
    , Error PlatformError
    , Embed IO
    ]

class Platform p => Exchange p e | e -> p where

    type SwapConfig e :: *

    fetchPrices
        :: Members
            ( Input e
            : PlatformEffects p
            ) r
        => [Asset] -> Sem r Prices
    
    swap
        :: Members (SwapEffects p e) r
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
