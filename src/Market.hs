{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Market
    ( module Market
    , module Market.Error
    , module Market.Types
    ) where

import Control.Monad.State
import Data.Record.Hom
import Market.Error
import Market.Types

class Monad m => MonadExecutor assets m | m -> assets where

    trade
        :: forall a1 a2. (Has a1 assets, Has a2 assets)
        => Asset a1 -> OrderAmount -> Asset a2 -> m ()

    stake :: forall a. Has a assets => Asset a -> OrderAmount -> m OrderId

    swap
        :: forall a1 a2. (Has a1 assets, Has a2 assets)
        => Asset a1 -> OrderAmount -> Asset a2 -> OrderAmount -> m OrderId

    disown :: forall a. Has a assets => Asset a -> OrderAmount -> m ()

    cancel :: OrderId -> m ()

class Instrument assets i where
    execute :: MonadExecutor assets m => Prices assets -> Portfolio assets -> StateT i m ()
