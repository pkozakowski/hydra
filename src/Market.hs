{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Market
    ( module Market
    , module Market.Types
    ) where

import Data.Proxy
import Data.Record.Hom
import Data.Time
import Market.Types
import Polysemy
import Polysemy.Error
import Polysemy.Internal
import Polysemy.Reader
import Polysemy.State as State

data Market assets m b where
    
    Trade :: LabelIn assets -> LabelIn assets -> OrderAmount -> Market assets m ()

    GetTime :: Market assets m UTCTime

    -- not implemented yet:

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

getTime
    :: forall assets r. (Member (Market assets) r) => Sem r UTCTime
getTime = send (GetTime :: Market assets (Sem r) UTCTime)

type InstrumentEffects assets c i
    = '[Market assets, Reader c, State i, Error String]

class Instrument assets c i | i -> c, c -> i where

    initState :: Prices assets -> c -> i

    initAllocation :: Prices assets -> c -> Distribution assets

    execute
        :: Members (InstrumentEffects assets c i) r
        => Prices assets -> Portfolio assets -> Sem r ()

runInstrument
    :: Instrument assets c i
    => Prices assets -> c -> Sem (Reader c ': State i ': r) a -> Sem r (i, a)
runInstrument prices config
    = runState (initState prices config) . runReader config

data SomeConfig assets = SomeConfig
    { someInitState :: Prices assets -> SomeInstrument assets
    , someInitAllocation :: Prices assets -> Distribution assets
    }

data SomeInstrument assets = SomeInstrument
    { someExecute
        :: forall r
        .  Members
            ( InstrumentEffects
                assets
                (SomeConfig assets)
                (SomeInstrument assets)
            ) r
        => Prices assets -> Portfolio assets -> Sem r ()
    }

instance Instrument assets (SomeConfig assets) (SomeInstrument assets) where
    initState prices cfg = someInitState cfg prices
    initAllocation prices cfg = someInitAllocation cfg prices
    execute prices portfolio = do
        dict <- State.get
        someExecute dict prices portfolio

someConfig
    :: forall assets c i
    .  Instrument assets c i
    => c -> SomeConfig assets
someConfig config = SomeConfig initSt initAlloc where
    initSt prices = someInstrument config $ initState @_ @_ @i prices config
    initAlloc prices = initAllocation @_ @_ @i prices config

someInstrument
    :: forall assets c i
    .  Instrument assets c i
    => c -> i -> SomeInstrument assets
someInstrument config instr = SomeInstrument exec where
    exec
        :: forall r
        .  Members
            ( InstrumentEffects
                assets
                (SomeConfig assets)
                (SomeInstrument assets)
            )
            r
        => Prices assets -> Portfolio assets -> Sem r ()
    exec prices portfolio = do
        instr'
            <- runReader config $ execState instr
            $  execute @_ @c @i prices portfolio
        put (someInstrument config instr' :: SomeInstrument assets)
