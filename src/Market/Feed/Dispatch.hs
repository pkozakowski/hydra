{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Market.Feed.Dispatch where

import Data.Function
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Static
import Market.Feed
import Market.Feed.DB
import Market.Feed.IBKR
import Market.Feed.Types
import Market.Types
import Numeric.Algebra.Division
import Polysemy
import Polysemy.Error
import Polysemy.Logging
import Prelude hiding (recip)

data PriceSpread = PriceSpread {bid :: Price, ask :: Price}
  deriving (Show)

-- TODO: make this the main Prices
newtype PriceSpreads = PriceSpreads {unPriceSpreads :: StaticMap Asset PriceSpread}
  deriving (Show, ReadMap Asset PriceSpread, BuildMap Asset PriceSpread)

runPriceSpreadFeed
  :: forall r a
   . Members [Error String, Logging, Final IO] r
  => DBPath
  -> Asset
  -> Sem (Feed PriceSpreads : r) a
  -> Sem r a
runPriceSpreadFeed dbPath baseAsset =
  push "runPriceSpreadFeed"
    . refeed assetsToIBKRKeys ibkrMapToPriceSpreads (runFeedWithDBCache dbPath runFeedIBKR)
  where
    assetsToIBKRKeys :: NonEmpty Asset -> NonEmpty ContractBarField
    assetsToIBKRKeys assets =
      assets >>= \asset ->
        ( \field ->
            ContractBarField
              { contract = Cash {symbol = unAsset baseAsset, currency = unAsset asset}
              , barField = field
              }
        )
          <$> allBarFields

    ibkrMapToPriceSpreads :: StaticMap ContractBarField Scalar -> PriceSpreads
    ibkrMapToPriceSpreads ibkrMap = fromList $ buildKV <$> groups
      where
        buildKV (ContractBarField (Cash {..}) _ :| _) =
          ( Asset currency
          , PriceSpread
              { bid = Price $ recip $ ibkrMap ! ContractBarField Cash {..} BidAvg
              , ask = Price $ recip $ ibkrMap ! ContractBarField Cash {..} AskAvg
              }
          )
        groups = NonEmpty.groupBy ((==) `on` contract) $ fst <$> toList ibkrMap
