module Market
    ( module Market
    , module Market.Error
    , module Market.Types
    ) where

import Control.Monad.State
import Market.Error
import Market.Types

class Instrument a where
    ownedAssets :: a -> AssetPortfolio
    updateAssets :: AssetPortfolio -> StateT a ErrorM ()
    makeOrders :: AssetPrices -> State a [Order]
