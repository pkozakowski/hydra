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
import Market.Feed.IBKR (runFeedIBKR)
import Market.Feed.IBKR qualified as IBKR
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

data FeedError = AssetPairNotFound AssetPair | UnexpectedError String
  deriving (Show)

runPriceSpreadFeed
  :: forall r a
   . Members [Error FeedError, Logging, Final IO] r
  => DBPath
  -> Asset
  -> Sem (Feed PriceSpreads : r) a
  -> Sem r a
runPriceSpreadFeed dbPath baseAsset action =
  push "runPriceSpreadFeed" do
    interpret interpreter action
  where
    interpreter :: forall rInitial x. Feed PriceSpreads (Sem rInitial) x -> Sem r x
    interpreter = \case
      Between1_ key period from to ->
        between1_UsingBetween_ (runPriceSpreadFeed dbPath baseAsset) key period from to
      Between_ (assets :: NonEmpty Asset) period from to -> do
        apct :: NonEmpty (AssetPair, IBKR.Contract, Scalar -> Scalar) <-
          forM assets \asset ->
            let assetPair =
                  AssetPair
                    { numerator = asset
                    , denominator = baseAsset
                    }
             in maybe
                  ( throw $
                      AssetPairNotFound assetPair
                  )
                  (\(c, t) -> pure (assetPair, c, t))
                  $ findContractAndPriceTranslation assetPair

        let addBarFields :: IBKR.Contract -> NonEmpty IBKR.ContractBarField
            addBarFields contract =
              ( \field ->
                  IBKR.ContractBarField
                    { contract = contract
                    , barField = field
                    }
              )
                <$> IBKR.allBarFields

            ibkrKeys = addBarFields . snd3 =<< apct

            c2t :: StaticMap IBKR.Contract (Scalar -> Scalar)
            c2t = Map.fromList $ NonEmpty.toList $ ((,) <$> snd3 <*> trd3) <$> apct

            c2ap :: StaticMap IBKR.Contract AssetPair
            c2ap = Map.fromList $ NonEmpty.toList $ ((,) <$> snd3 <*> fst3) <$> apct

            fst3 (x, _, _) = x
            snd3 (_, x, _) = x
            trd3 (_, _, x) = x

            ibkrMapToPriceSpreads :: StaticMap IBKR.ContractBarField Scalar -> PriceSpreads
            ibkrMapToPriceSpreads ibkrMap = fromList $ buildKV <$> groups
              where
                buildKV (IBKR.ContractBarField contract@IBKR.Contract {..} _ :| _) =
                  ( Asset $ T.unpack currency
                  , PriceSpread
                      { bid = Price $ (c2t ! contract) $ ibkrMap ! IBKR.ContractBarField contract IBKR.BidAvg
                      , ask = Price $ (c2t ! contract) $ ibkrMap ! IBKR.ContractBarField contract IBKR.AskAvg
                      }
                  )
                groups = NonEmpty.groupBy ((==) `on` IBKR.contract) $ fst <$> toList ibkrMap

        let translateError = \case
              IBKR.ContractNotFound c -> AssetPairNotFound $ c2ap ! c
              err -> UnexpectedError $ show err

        mapFeedSeries_ ibkrMapToPriceSpreads
          $ mapErrorWithLog translateError
          $ runFeedWithDBCache @(StaticMap IBKR.ContractBarField Scalar)
            dbPath
            -- (mapErrorWithLog translateError . runFeedIBKR . raiseUnder)
            runFeedIBKR
          $ raiseUnder
          $ between_ ibkrKeys period from to

data AssetPair = AssetPair {numerator :: Asset, denominator :: Asset}
  deriving (Show, Read, Eq, Ord)

findContractAndPriceTranslation :: AssetPair -> Maybe (IBKR.Contract, Scalar -> Scalar)
findContractAndPriceTranslation assetPair =
  case Map.lookup assetPair knownContracts of
    Just contract -> Just (contract, id)
    Nothing -> case Map.lookup (flip assetPair) knownContracts of
      Just contract -> Just (contract, recip)
      Nothing -> Nothing
  where
    flip AssetPair {..} = AssetPair {numerator = denominator, denominator = numerator}

-- TODO: move to Dhall
knownContracts :: StaticMap AssetPair IBKR.Contract
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

cash :: Asset -> Asset -> (AssetPair, IBKR.Contract)
cash numerator denominator =
  ( AssetPair {..}
  , IBKR.Contract
      { symbol = T.pack $ unAsset numerator
      , currency = T.pack $ unAsset denominator
      , type_ = IBKR.Cash
      , exchange = IBKR.IdealPro
      }
  )
