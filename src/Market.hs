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

type InstrumentEffects assets r s
    = '[Market assets, Reader r, State s, Error String]

class Instrument assets c s | s -> c, c -> s where

    initState :: Prices assets -> c -> s

    initAllocation :: Prices assets -> c -> Distribution assets

    execute
        :: Members (InstrumentEffects assets c s) r
        => Prices assets -> Portfolio assets -> Sem r ()

runInstrument
    :: Instrument assets c s
    => Prices assets -> c -> Sem (Reader c ': State s ': r) a -> Sem r (s, a)
runInstrument prices config
    = runState (initState prices config) . runReader config

data SomeInstrumentConfig assets = SomeInstrumentConfig
    { someInitState :: Prices assets -> SomeInstrumentState assets
    , someInitAllocation :: Prices assets -> Distribution assets
    , someShow :: String
    }

data SomeInstrumentState assets = SomeInstrumentState
    { someExecute
        :: forall r
        .  Members
            ( InstrumentEffects
                assets
                (SomeInstrumentConfig assets)
                (SomeInstrumentState assets)
            ) r
        => Prices assets -> Portfolio assets -> Sem r ()
    }

instance Instrument assets (SomeInstrumentConfig assets) (SomeInstrumentState assets) where
    initState prices cfg = someInitState cfg prices
    initAllocation prices cfg = someInitAllocation cfg prices
    execute prices portfolio = do
        dict <- State.get
        someExecute dict prices portfolio

instance Show (SomeInstrumentConfig assets) where
    show = someShow

someInstrumentConfig
    :: forall assets c s
    .  (Instrument assets c s, Show c)
    => c -> SomeInstrumentConfig assets
someInstrumentConfig config = SomeInstrumentConfig initSt initAlloc shw where
    initSt prices = someInstrumentState config $ initState @_ @_ @s prices config
    initAlloc prices = initAllocation @_ @_ @s prices config
    shw = show config

someInstrumentState
    :: forall assets c s
    .  Instrument assets c s
    => c -> s -> SomeInstrumentState assets
someInstrumentState config instr = SomeInstrumentState exec where
    exec
        :: forall r
        .  Members
            ( InstrumentEffects
                assets
                (SomeInstrumentConfig assets)
                (SomeInstrumentState assets)
            )
            r
        => Prices assets -> Portfolio assets -> Sem r ()
    exec prices portfolio = do
        instr'
            <- runReader config $ execState instr
            $  execute @_ @c @s prices portfolio
        put (someInstrumentState config instr' :: SomeInstrumentState assets)
