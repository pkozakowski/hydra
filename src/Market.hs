module Market
    ( module Market
    , module Market.Error
    , module Market.Types
    ) where

import Control.Monad.State
import Market.Error
import Market.Types

class MonadExecutor m where
    trade :: Asset -> Asset -> OrderAmount -> m ()
    tradeLimit :: Asset -> Asset -> OrderAmount -> Price -> m OrderId
    stake :: Asset -> OrderAmount -> m OrderId
    swap :: Asset -> OrderAmount -> Asset -> OrderAmount -> m OrderId
    disown :: Asset -> OrderAmount -> m ()
    cancel :: OrderId -> m ()

class Instrument a where
    -- TODO: Make type-safe using extensible records?
    updateAssets :: AssetPortfolio -> StateT a ErrorM ()
    execute :: MonadExecutor m => AssetPrices -> StateT a m ()
