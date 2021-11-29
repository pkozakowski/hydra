{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module Market.Instrument.Dhall where

import Data.Bifunctor
import Data.Either.Validation
import Data.Functor.Identity
import Data.Text (pack, unpack)
import Dhall
import Dhall.TH
import Market
import Market.Dhall
import Market.Instrument.Balance
import Market.Instrument.Hold
import Market.Instrument.Some

configurableInstruments :: [(String, Decoder SomeInstrumentConfig)]
configurableInstruments =
    [ configure @BalanceConfig "balance"
    , configure @Hold "hold"
    ] where
        configure
            :: forall c s
             . (FromDhall c, Show c, Instrument c s)
            => String
            -> (String, Decoder SomeInstrumentConfig)
        configure name = (name, someInstrumentConfig <$> auto @c)

deriving instance FromDhall BalanceConfig

instance FromDhall SomeInstrumentConfig where

    autoWith _ = Decoder { .. } where

        expected = pure [dhall|
            let Instrument = ./dhall/Market/Instrument/Type
            in ./dhall/Market/Instrument/Builder Instrument
                    -> Instrument
            |]

        extract = extractRecursive "Instrument" expected \instrumentType
            -> lookup (unpack instrumentType) configurableInstruments
