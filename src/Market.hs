{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Market
    ( module Market
    , module Market.Error
    , module Market.Types
    ) where

import Data.Record.Hom
import Control.Monad.State
import Market.Error
import Market.Types

class MonadExecutor assets m | m -> assets where

    trade :: (Has a1 assets, Has a2 assets) => Asset a1 -> OrderAmount -> Asset a2 -> m ()

    stake :: Has a assets => Asset a -> OrderAmount -> m OrderId

    swap
        :: (Has a1 assets, Has a2 assets)
        => Asset a1 -> OrderAmount -> Asset a2 -> OrderAmount -> m OrderId

    disown :: Has a assets => Asset a -> OrderAmount -> m ()

    cancel :: OrderId -> m ()

class Instrument assets i | i -> assets where
    execute :: MonadExecutor assets m => Prices assets -> Portfolio assets -> StateT i m ()
