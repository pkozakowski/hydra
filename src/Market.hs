{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Market
    ( module Market
    , module Market.Types
    ) where

import Control.DeepSeq
import Data.Fixed
import Data.Proxy
import Data.Record.Hom
import GHC.Generics
import Market.Time
import Market.Types
import Numeric.Truncatable
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Internal
import Polysemy.State as State

data Market assets m a where

    Trade :: LabelIn assets -> LabelIn assets -> OrderAmount
          -> Market assets m ()

makeSem_ ''Market

trade
    :: forall assets r
    .  (Member (Market assets) r)
    => LabelIn assets -> LabelIn assets -> OrderAmount -> Sem r ()

newtype IConfig c = IConfig c

newtype IState s = IState s
    deriving newtype (Truncatable)

data MarketError assets
    = InsufficientBalanceForTransfer (SomeAmount assets)
    | InsufficientBalanceToCoverFees (Fees assets)
    | OtherError String
    deriving (Generic, NFData)

instance Show (MarketError assets) where

    show = \case
        InsufficientBalanceForTransfer someAmount
            -> "insufficient balance for transfer: " ++ show someAmount
        InsufficientBalanceToCoverFees fees
            -> "insufficient balance to cover fees: " ++ show fees
        OtherError s -> s

type InitEffects assets c =
    [ Input (Prices assets)
    , Input (IConfig c)
    ]

type ExecuteEffects assets c s =
    [ Market assets
    , Time
    , Input (Fees assets)
    , Input (Prices assets)
    , Input (Portfolio assets)
    , Input (IConfig c)
    , State (IState s)
    , Error (MarketError assets)
    ]

type AggregateVisitor self agg
    = forall sis
    . self -> HomRec sis agg -> agg

type SelfVisitor assets self
     = forall c' s'
     . Instrument assets c' s'
    => Prices assets
    -> Portfolio assets
    -> c' -> s' -> self

type Visitor assets self agg
     = AggregateVisitor self agg
    -> SelfVisitor assets self
    -> agg

class Truncatable s => Instrument assets c s | s -> c, c -> s where

    initState :: Members (InitEffects assets c) r => Sem r s

    initAllocation
        :: Members (InitEffects assets c) r
        => Sem r (Distribution assets)

    execute
        :: Members (ExecuteEffects assets c s) r
        => Sem r ()

    visit
        :: Prices assets
        -> Portfolio assets
        -> c
        -> s
        -> Visitor assets self agg

visit'
    :: forall assets self agg c s
     . Instrument assets c s
    => AggregateVisitor self agg
    -> SelfVisitor assets self
    -> Prices assets
    -> Portfolio assets
    -> c
    -> s
    -> agg
visit' visitAgg visitSelf prices portfolio config state
    = visit @assets prices portfolio config state visitAgg visitSelf

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
            ( ExecuteEffects
                assets
                (SomeInstrumentConfig assets)
                (SomeInstrumentState assets)
            ) r
        => Sem r ()
    , someVisit
        :: forall self agg
         . Prices assets
        -> Portfolio assets
        -> Visitor assets self agg
    , someTruncateTo
        :: forall res
         . HasResolution res
        => res
        -> SomeInstrumentState assets
    }

instance Truncatable (SomeInstrumentState assets) where
    truncateTo = flip someTruncateTo

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

    visit prices portfolio _ state = someVisit state prices portfolio

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
someInstrumentState config state = SomeInstrumentState exec vis trunc where
    exec
        :: forall r
        .  Members
            ( ExecuteEffects
                assets
                (SomeInstrumentConfig assets)
                (SomeInstrumentState assets)
            )
            r
        => Sem r ()
    exec = do
        IState state'
           <- runInputConst (IConfig config) $ execState (IState state)
            $ execute @assets @c @s
        put @(IState (SomeInstrumentState assets))
            $ IState $ someInstrumentState config state'

    vis :: Prices assets -> Portfolio assets -> Visitor assets self agg
    vis prices portfolio = visit prices portfolio config state

    trunc :: forall res. HasResolution res => res -> SomeInstrumentState assets
    trunc res = someInstrumentState config $ truncateTo res state
