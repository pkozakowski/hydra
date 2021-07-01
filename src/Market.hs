{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Market
    ( module Market
    , module Market.Error
    , module Market.Types
    ) where

import Data.Proxy
import Data.Record.Hom
import Market.Error
import Market.Types
import Polysemy
import Polysemy.Internal
import Polysemy.State

data Market assets m b where
    
    Trade :: LabelIn assets -> LabelIn assets -> OrderAmount -> Market assets m ()

    Stake :: Has a assets => Asset a -> OrderAmount -> Market assets m OrderId

    Swap
        :: (Has a1 assets, Has a2 assets)
        => Asset a1 -> OrderAmount -> Asset a2 -> OrderAmount -> Market assets m OrderId

    Disown :: Has a assets => Asset a -> OrderAmount -> Market assets m ()

    Cancel :: OrderId -> Market assets m ()

trade
    :: forall assets r
    .  (Member (Market assets) r)
    => LabelIn assets -> LabelIn assets -> OrderAmount -> Sem r ()
trade from to amount
    = send (Trade from to amount :: Market assets (Sem r) ()) where

class Instrument assets i where

    execute
        :: Members '[State i, Market assets] r
        => Prices assets -> Portfolio assets -> Sem r ()
