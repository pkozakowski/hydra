{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}

module Market.Instrument.Hold where

import Data.Map.Static
import Data.List.NonEmpty
import Data.Text (Text)
import Data.Void
import Dhall (FromDhall)
import qualified Dhall as Dh
import qualified Dhall.Core as Dh
import GHC.Generics
import Market
import Market.Dhall
import Market.Instrument.Ops
import Numeric.Algebra
import Numeric.Truncatable
import Polysemy.Input

data Hold = Hold { held :: Asset }
    deriving (FromDhall, Generic, Show)

instance Truncatable Hold where
    truncateTo _ = id

instance Instrument Hold Hold where

    initState = unIConfig <$> input

    initAllocation = onePoint . held . unIConfig <$> input

    execute = do
        IConfig (Hold asset) <- input
        allocationToTrades zero $ onePoint asset

    visit prices portfolio config state visitAgg visitSelf
        = visitAgg (visitSelf prices portfolio config state) empty

    managedAssets config = [held config]

    smartEmbed
        = Dh.MultiLet
            ( import_ ["Market", "Instrument", "Hold"] "hold"
           :| import_ ["Market"] "asset"
            : []
            )
        . call "hold"
        . call "asset"
        . fmap absurd
        . Dh.embed Dh.inject
        . unAsset
        . held
