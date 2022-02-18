{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Market.Blockchain where

import Control.Monad
import Data.ByteString
import Data.Constraint
import Data.Fixed
import Data.Map.Class
import Market
import Numeric.Kappa
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Reader
import Prelude hiding (pi)
import Type.List

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

type PlatformEffects p =
    [ Input p
    , Reader (PlatformConfig p)
    , Error PlatformError
    , Embed IO
    ] ++ Effects p

type TransactionEffects p =
    [ Input p
    , Reader (PlatformConfig p)
    , Error TransactionError
    , Error PlatformError
    , Embed IO
    ] ++ Effects p

class Platform p where

    type Effects p :: EffectRow
    type Wallet p :: *
    type PlatformConfig p :: *

    loadWallet
        :: Members (PlatformEffects p) r
        => ByteString -> Sem r (Wallet p)

    fetchBalance
        :: Members
            ( Input (Wallet p)
            : PlatformEffects p
            ) r
        => Asset -> Sem r Amount

    fetchPortfolio
        :: Members
            ( Input (Wallet p)
            : PlatformEffects p
            ) r
        => [Asset] -> Sem r Portfolio
    fetchPortfolio assets = do
        assetsAndAmounts <- forM assets \asset
            -> (asset,) <$> fetchBalance @p asset
        pure $ fromList assetsAndAmounts

    runPlatform
        :: Member (Embed IO) r
        => p
        -> PlatformConfig p
        -> Sem
            ( Input p
            : Reader (PlatformConfig p)
            : Effects p
           ++ r
            ) a
        -> Sem r a

data SwapError
    = PriceSlipped
    deriving Show

type SwapEffects p e =
    [ Input p
    , Input (Wallet p)
    , Reader (PlatformConfig p)
    , Input e
    , Input (SwapConfig e)
    , Error SwapError
    , Error TransactionError
    , Error PlatformError
    , Embed IO
    ] ++ Effects p

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

runMarketBlockchain
    :: forall p e r a
     . (Platform p, Exchange p e, Members (SwapEffects p e) r)
    => Sem (Market : r) a
    -> Sem r a
runMarketBlockchain = interpret \case
    Trade from to orderAmount -> do
        amount <- case orderAmount of
            Absolute am -> return am
            Relative shr -> do
                fromBalance <- fetchBalance @p @r from
                return $ fromBalance `pi` shr
        swap @p @e from to amount

runInputPortfolioBlockchain
    :: forall p r a
     .  ( Platform p
        , Members (Input [Asset] : Input (Wallet p) : PlatformEffects p) r
        )
    => Sem (Input Portfolio : r) a
    -> Sem r a
runInputPortfolioBlockchain action = do
    assets <- input @[Asset] @r
    runInputSem (fetchPortfolio @p assets) action

runInputPricesBlockchain
    :: forall p e r a
     .  ( Platform p
        , Exchange p e
        , Members (Input [Asset] : Input e : PlatformEffects p) r
        )
    => Sem (Input Prices : r) a
    -> Sem r a
runInputPricesBlockchain action = do
    assets <- input @[Asset] @r
    runInputSem (fetchPrices @p @e assets) action
