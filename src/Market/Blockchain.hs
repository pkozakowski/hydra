{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Market.Blockchain where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Constraint
import Data.Fixed
import Data.Map.Class
import Data.Time
import GHC.Natural
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
    | RetryableTransactionTimeout
    | TransactionTimeout
    | UnknownTransactionError String
    deriving Show

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

type BlockNumber = Natural

data Block = Latest | Nth BlockNumber

class Platform p where

    type Effects p :: EffectRow
    type Wallet p :: *
    type PlatformConfig p :: *

    loadWallet
        :: Members (PlatformEffects p) r
        => ByteString -> Sem r (Wallet p)

    fetchLatestBlock
        :: Members (PlatformEffects p) r
        => Sem r BlockNumber

    fetchBlockTime
        :: Members (PlatformEffects p) r
        => BlockNumber -> Sem r UTCTime

    fetchBalanceAt
        :: Members
            ( Input (Wallet p)
            : PlatformEffects p
            ) r
        => Block -> Asset -> Sem r Amount

    fetchPortfolioAt
        :: Members
            ( Input (Wallet p)
            : PlatformEffects p
            ) r
        => Block -> [Asset] -> Sem r Portfolio
    fetchPortfolioAt block assets = do
        assetsAndAmounts <- forM assets \asset
            -> (asset,) <$> fetchBalanceAt @p block asset
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

    fetchPricesAt
        :: Members
            ( Input e
            : PlatformEffects p
            ) r
        => Block -> [Asset] -> Sem r Prices

    estimateFees
        :: Members
            ( Input e
            : PlatformEffects p
            ) r
        => Sem r Fees

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

fetchBalance
    :: forall p r
     .  ( Platform p
        , Members
             ( Input (Wallet p)
             : PlatformEffects p
             ) r
        )

    => Asset -> Sem r Amount
fetchBalance = fetchBalanceAt @p @r Latest

fetchPortfolio
    :: forall p r
     .  ( Platform p
        , Members
            ( Input (Wallet p)
            : PlatformEffects p
            ) r
        )
    => [Asset] -> Sem r Portfolio
fetchPortfolio = fetchPortfolioAt @p @r Latest

fetchPrices
    :: forall p e r
     .  ( Exchange p e
        , Members
            ( Input e
            : PlatformEffects p
            ) r
        )
    => [Asset] -> Sem r Prices
fetchPrices = fetchPricesAt @p @e @r Latest

-- Interpolation search! TODO: Move to Ops and test.
findBlock
    :: forall p r
     .  ( Platform p
        , Members (PlatformEffects p) r
        )
    => UTCTime -> BlockNumber
    -> UTCTime -> BlockNumber
    -> UTCTime
    -> Sem r BlockNumber
findBlock beginTime beginBlock endTime endBlock time
    | min time endTime <= beginTime || endBlock - beginBlock <= 1
        = pure beginBlock
    | time >= endTime = pure endBlock
    | otherwise = do
        midTime <- fetchBlockTime @p @r midBlock
        if midTime < time
            then findBlock @p @r midTime midBlock endTime endBlock time
            else findBlock @p @r beginTime beginBlock midTime midBlock time
        where
            midBlock
              = correct $ round $ fromIntegral beginBlock + offset * slope
            offset = time `diffUTCTime` beginTime
            slope
              = fromIntegral (endBlock - beginBlock)
                  / (endTime `diffUTCTime` beginTime)
            correct midBlock
              | midBlock <= beginBlock =
                beginBlock + 1
              | midBlock >= endBlock = endBlock - 1
              | otherwise = midBlock

findBlocks
    :: forall p r
     .  ( Platform p
        , Members
            (PlatformEffects p) r
        )
    => [UTCTime] -> Sem r [BlockNumber]
findBlocks times = do
    earliestTime <- fetchBlockTime @p @r 0
    latestBlock <- fetchLatestBlock @p @r
    latestTime <- fetchBlockTime @p @r latestBlock
    headBlock <- findBlock @p @r
        earliestTime earliestBlock latestTime latestBlock headTime
    lastBlock <- findBlock @p @r
        headTime headBlock latestTime latestBlock lastTime
    let find ((time, block), blocks) time' = do
            block' <- findBlock @p @r time block lastTime lastBlock time'
            pure ((time', block'), block' : blocks)
    middleBlocks
       <- fmap (reverse . snd)
        $ foldM find ((headTime, headBlock), [])
        $ tail $ init times
    pure $ headBlock : middleBlocks ++ [lastBlock]
    where
        earliestBlock = 0
        headTime = head times
        lastTime = last times

fetchPortfoliosOver
    :: forall p r
     .  ( Platform p
        , Members
            ( Input (Wallet p)
            : PlatformEffects p
            ) r
        )
    => [UTCTime] -> [Asset] -> Sem r Portfolio
fetchPortfoliosOver = error "TODO"

fetchPricesOver
    :: forall p e r
     .  ( Exchange p e
        , Members
            ( Input e
            : PlatformEffects p
            ) r
        )
    => [UTCTime] -> [Asset] -> Sem r Prices
fetchPricesOver = error "TODO"
