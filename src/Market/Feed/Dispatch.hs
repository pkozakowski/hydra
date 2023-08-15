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

data SpreadPrice = SpreadPrice {bid :: Price, ask :: Price}
  deriving (Show)

-- TODO: make this the main Prices
newtype SpreadPrices = SpreadPrices {unSpreadPrices :: StaticMap Asset SpreadPrice}
  deriving (Show, ReadMap Asset SpreadPrice, BuildMap Asset SpreadPrice)

runSpreadPriceFeed
  :: forall r a
   . Members [Error String, Logging, Final IO] r
  => DBPath
  -> Asset
  -> Sem (Feed SpreadPrices : r) a
  -> Sem r a
runSpreadPriceFeed dbPath baseAsset =
  refeed assetsToIBKRKeys ibkrMapToSpreadPrices $ runFeedWithDBCache dbPath runFeedIBKR
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

    ibkrMapToSpreadPrices :: StaticMap ContractBarField Scalar -> SpreadPrices
    ibkrMapToSpreadPrices ibkrMap = fromList $ buildKV <$> groups
      where
        buildKV (ContractBarField (Cash {..}) _ :| _) =
          ( Asset currency
          , SpreadPrice
              { bid = Price $ recip $ ibkrMap ! ContractBarField Cash {..} BidAvg
              , ask = Price $ recip $ ibkrMap ! ContractBarField Cash {..} AskAvg
              }
          )
        groups = NonEmpty.groupBy ((==) `on` contract) $ fst <$> toList ibkrMap
