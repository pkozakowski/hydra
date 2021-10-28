{-# LANGUAGE DeriveAnyClass #-}

module Market.Instrument.Hold where

import Data.Map.Static
import Dhall (FromDhall)
import GHC.Generics
import Market
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
