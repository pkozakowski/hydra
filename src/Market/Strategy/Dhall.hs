{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module Market.Strategy.Dhall where

import Data.Bifunctor
import Data.Either.Validation
import Data.Functor.Identity
import Data.Text (pack, unpack)
import Dhall
import Dhall.TH
import Market
import Market.Dhall
import Market.Strategy.Balance
import Market.Strategy.Hold
import Market.Strategy.Some

configurableStrategies :: [(String, Decoder SomeStrategyConfig)]
configurableStrategies =
  [ configure @BalanceConfig "balance"
  , configure @Hold "hold"
  ]
  where
    configure
      :: forall c s
       . (FromDhall c, Show c, Strategy c s)
      => String
      -> (String, Decoder SomeStrategyConfig)
    configure name = (name, someStrategyConfig <$> auto @c)

deriving instance FromDhall BalanceConfig

instance FromDhall SomeStrategyConfig where
  autoWith _ = Decoder {..}
    where
      expected =
        pure
          [dhall|
            let Strategy = ./dhall/Market/Strategy/Type
            in ./dhall/Market/Strategy/Builder Strategy
                    -> Strategy
            |]

      extract = extractRecursive "Strategy" expected \strategyType ->
        lookup (unpack strategyType) configurableStrategies
