{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}

module Market.Strategy.Hold where

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
import Market.Strategy.Ops
import Numeric.Algebra
import Numeric.Truncatable
import Polysemy.Input

data Hold = Hold { held :: Asset }
    deriving (FromDhall, Generic, Show)

instance Truncatable Hold where
    truncateTo _ = id

instance Strategy Hold Hold where

    initState = unSConfig <$> input

    initAllocation = onePoint . held . unSConfig <$> input

    execute = do
        SConfig (Hold asset) <- input
        allocationToTrades zero $ onePoint asset

    visit prices portfolio config state visitAgg visitSelf
        = visitAgg (visitSelf prices portfolio config state) empty

    managedAssets config = [held config]

    smartEmbed
        = Dh.MultiLet
            ( import_ ["Market", "Strategy", "Hold"] "hold"
           :| import_ ["Market"] "asset"
            : []
            )
        . call "hold"
        . call "asset"
        . fmap absurd
        . Dh.embed Dh.inject
        . unAsset
        . held
