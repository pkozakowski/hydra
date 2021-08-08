{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Market
    ( module Market
    , module Market.Types
    ) where

import Data.Proxy
import Data.Record.Hom
import Market.Time
import Market.Types
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Internal
import Polysemy.State as State

data Market assets m a where

    Trade :: LabelIn assets -> LabelIn assets -> OrderAmount
          -> Market assets m ()

trade
    :: forall assets r
    .  (Member (Market assets) r)
    => LabelIn assets -> LabelIn assets -> OrderAmount -> Sem r ()
trade from to amount
    = send (Trade from to amount :: Market assets (Sem r) ()) where

newtype IConfig c = IConfig c
newtype IState c = IState c

type InstrumentInitEffects assets c
    = '[Input (Prices assets), Input (IConfig c)]

type InstrumentEffects assets c s =
   '[ Market assets
    , Time
    , Input (Prices assets)
    , Input (Portfolio assets)
    , Input (IConfig c)
    , State (IState s)
    , Error String
    ]

class Instrument assets c s | s -> c, c -> s where

    initState :: Members (InstrumentInitEffects assets c) r => Sem r s

    initAllocation
        :: Members (InstrumentInitEffects assets c) r
        => Sem r (Distribution assets)

    execute
        :: Members (InstrumentEffects assets c s) r
        => Sem r ()

runInstrument
    :: forall assets c s r a
     . (Instrument assets c s, Member (Input (Prices assets)) r)
    => c -> Sem (State (IState s) ': Input (IConfig c) ': r) a -> Sem r (s, a)
runInstrument config monad = do
    state <- runInputConst (IConfig config) $ initState @assets
    runInstrument' @assets config state monad

runInstrument'
    :: forall assets c s r a
     . (Instrument assets c s, Member (Input (Prices assets)) r)
    => c -> s -> Sem (State (IState s) ': Input (IConfig c) ': r) a
    -> Sem r (s, a)
runInstrument' config state monad = runInputConst (IConfig config) do
    (IState state', x) <- runState (IState state) monad
    return (state', x)

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
        => Sem r ()
    }

instance
    Instrument
        assets
        (SomeInstrumentConfig assets)
        (SomeInstrumentState assets)
    where

    initState = do
        IConfig config <- input
        prices <- input
        return $ someInitState config prices

    initAllocation = do
        IConfig config <- input
        prices <- input
        return $ someInitAllocation config prices

    execute = do
        IState state <- State.get @(IState (SomeInstrumentState assets))
        someExecute state

instance Show (SomeInstrumentConfig assets) where
    show = someShow

someInstrumentConfig
    :: forall assets c s
    .  (Instrument assets c s, Show c)
    => c -> SomeInstrumentConfig assets
someInstrumentConfig config = SomeInstrumentConfig initSt initAlloc shw where
    initSt prices
        = someInstrumentState config
        $ run $ runInputConst prices $ runInputConst (IConfig config)
        $ initState @assets
    initAlloc prices
        = run $ runInputConst prices $ runInputConst (IConfig config)
        $ initAllocation @assets @c
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
        => Sem r ()
    exec = do
        IState instr'
           <- runInputConst (IConfig config) $ execState (IState instr)
            $ execute @assets @c @s
        put @(IState (SomeInstrumentState assets))
            $ IState $ someInstrumentState config instr'
