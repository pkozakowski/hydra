{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Market.Feed.Dispatch where

import Control.Monad
import Data.Function
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Class qualified as Map
import Data.Map.Static
import Data.Text qualified as T
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
    . refeed refeedF (runFeedWithDBCache dbPath runFeedIBKR)
  where
    refeedF
      :: NonEmpty Asset
      -> Sem
          r
          ( NonEmpty ContractBarField
          , StaticMap ContractBarField Scalar -> PriceSpreads
          )
    refeedF assets = do
      ct :: NonEmpty (Contract, Scalar -> Scalar) <-
        forM assets \asset ->
          maybe
            (throw $ "contract not found for " <> show asset <> " " <> show baseAsset)
            pure
            $ findContractAndPriceTranslation
            $ AssetPair
              { numerator = asset
              , denominator = baseAsset
              }

      let addBarFields :: Contract -> NonEmpty ContractBarField
          addBarFields contract =
            ( \field ->
                ContractBarField
                  { contract = contract
                  , barField = field
                  }
            )
              <$> allBarFields

          ibkrKeys = addBarFields . fst =<< ct

          c2t :: StaticMap Contract (Scalar -> Scalar)
          c2t = Map.fromList $ NonEmpty.toList ct

          ibkrMapToPriceSpreads :: StaticMap ContractBarField Scalar -> PriceSpreads
          ibkrMapToPriceSpreads ibkrMap = fromList $ buildKV <$> groups
            where
              buildKV (ContractBarField contract@Contract {..} _ :| _) =
                ( Asset $ T.unpack currency
                , PriceSpread
                    { bid = Price $ (c2t ! contract) $ ibkrMap ! ContractBarField contract BidAvg
                    , ask = Price $ (c2t ! contract) $ ibkrMap ! ContractBarField contract AskAvg
                    }
                )
              groups = NonEmpty.groupBy ((==) `on` contract) $ fst <$> toList ibkrMap

      pure (ibkrKeys, ibkrMapToPriceSpreads)

data AssetPair = AssetPair {numerator :: Asset, denominator :: Asset}
  deriving (Show, Read, Eq, Ord)

findContractAndPriceTranslation :: AssetPair -> Maybe (Contract, Scalar -> Scalar)
findContractAndPriceTranslation assetPair =
  case Map.lookup assetPair knownContracts of
    Just contract -> Just (contract, id)
    Nothing -> case Map.lookup (flip assetPair) knownContracts of
      Just contract -> Just (contract, recip)
      Nothing -> Nothing
  where
    flip AssetPair {..} = AssetPair {numerator = denominator, denominator = numerator}

-- TODO: move to Dhall
knownContracts :: StaticMap AssetPair Contract
knownContracts =
  fromList
    [ cash "USD" "CNH"
    , cash "USD" "KRW"
    , cash "USD" "MXN"
    , cash "USD" "SGD"
    , cash "USD" "CAD"
    , cash "USD" "ZAR"
    , cash "USD" "JPY"
    , cash "USD" "CHF"
    , cash "USD" "NOK"
    , cash "USD" "AED"
    , cash "USD" "ILS"
    , cash "USD" "HUF"
    , cash "USD" "RUB"
    , cash "USD" "CZK"
    , cash "USD" "DKK"
    , cash "USD" "BGN"
    , cash "USD" "RON"
    , cash "USD" "PLN"
    , cash "USD" "SAR"
    , cash "USD" "TRY"
    , cash "USD" "HKD"
    , cash "USD" "SEK"
    ]

cash :: Asset -> Asset -> (AssetPair, Contract)
cash numerator denominator =
  ( AssetPair {..}
  , Contract
      { symbol = T.pack $ unAsset numerator
      , currency = T.pack $ unAsset denominator
      , type_ = Cash
      , exchange = IdealPro
      }
  )
