module Market
    ( module Market
    , module Market.Error
    , module Market.Types
    ) where

import Control.Monad.State
import Market.Error
import Market.Types

class MonadExecutor m where
    trade :: Asset -> Asset -> Amount -> m ()
    tradeLimit :: Asset -> Asset -> Amount -> Price -> m OrderId
    stake :: Asset -> Amount -> m OrderId
    swap :: Asset -> Amount -> Asset -> Amount -> m OrderId
    disown :: Asset -> Amount -> m ()
    cancel :: OrderId -> m ()

class Instrument a where
    ownedAssets :: a -> AssetPortfolio
    -- TODO: Make type-safe using extensible records?
    updateAssets :: AssetPortfolio -> StateT a ErrorM ()
    execute :: MonadExecutor m => AssetPrices -> StateT a m ()
