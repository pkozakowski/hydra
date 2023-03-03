{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Market.Broker where

import Data.Map.Class
import Data.Time
import Data.Time.Clock
import Data.Traversable
import Dhall (FromDhall)
import qualified Dhall as Dh
import GHC.Generics
import Market
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Reader
import Type.List

data BrokerError
    = NoSuchAsset Asset
    | TransportError String
    | LocalError String
    | UnknownBrokerError String
    deriving Show

type BrokerEffects b =
    [ Reader b
    , Error BrokerError
    , Embed IO
    ] ++ Effects b

class FromDhall b => Broker b where

    type Effects b :: EffectRow

    fetchPricesAt
        :: Members
            ( BrokerEffects b
            ) r
        => UTCTime -> [Asset] -> Sem r Prices

    fetchBalanceAt
        :: Members
            ( BrokerEffects b
            ) r
        => UTCTime -> Asset -> Sem r Amount

    fetchPortfolioAt
        :: Members
            ( BrokerEffects b
            ) r
        => UTCTime -> [Asset] -> Sem r Portfolio
    fetchPortfolioAt block assets = do
        assetsAndAmounts <- forM assets \asset
            -> (asset,) <$> fetchBalanceAt @b block asset
        pure $ fromList assetsAndAmounts

    estimateFees
        :: Members
            ( BrokerEffects b
            ) r
        => Sem r Fees

    runBroker
        :: Member (Embed IO) r
        => b
        -> Sem
            ( Reader b
            : Effects b
           ++ r
            ) a
        -> Sem r a

fetchPortfolio
    :: forall b r
     .  ( Broker b
        , Members
            ( BrokerEffects b
            ) r
        )
    => [Asset] -> Sem r Portfolio
fetchPortfolio assets = do
    time <- embed getCurrentTime
    fetchPortfolioAt @b @r time assets

fetchPrices
    :: forall b r
     .  ( Broker b
        , Members
            ( BrokerEffects b
            ) r
        )
    => [Asset] -> Sem r Prices
fetchPrices assets = do
    time <- embed getCurrentTime
    fetchPricesAt @b @r time assets

runMarketBroker
    :: forall b r a
     . (Broker b)
    => Sem (Market : r) a
    -> Sem r a
runMarketBroker = interpret \case
    Trade from to orderAmount -> do
        -- TODO
        pure ()

runInputPortfolioBroker
    :: forall b r a
     .  ( Broker b
        , Members (Input [Asset] : BrokerEffects b) r
        )
    => Sem (Input Portfolio : r) a
    -> Sem r a
runInputPortfolioBroker action = do
    assets <- input @[Asset] @r
    runInputSem (fetchPortfolio @b assets) action

runInputPricesBroker
    :: forall b r a
     .  ( Broker b
        , Members (Input [Asset] : BrokerEffects b) r
        )
    => Sem (Input Prices : r) a
    -> Sem r a
runInputPricesBroker action = do
    assets <- input @[Asset] @r
    runInputSem (fetchPrices @b assets) action

data DummyBroker = DummyBroker
    deriving (FromDhall, Generic)

instance Broker DummyBroker where
    type Effects DummyBroker = '[]
    fetchPricesAt = error "not implemented"
    fetchBalanceAt = error "not implemented"
    estimateFees = error "not implemented"
    runBroker = error "not implemented"
