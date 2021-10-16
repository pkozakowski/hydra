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
import Data.Map.Static
import Data.Proxy
import GHC.Generics
import Market.Time
import Market.Types
import Numeric.Truncatable
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Internal
import Polysemy.State as State

data Market m a where
    Trade :: Asset -> Asset -> OrderAmount -> Market m ()

makeSem ''Market

newtype IConfig c = IConfig { unIConfig :: c }

newtype IState s = IState { unIState :: s }
    deriving newtype (Truncatable)

data MarketError
    = InsufficientBalanceForTransfer SomeAmount Portfolio
    | InsufficientBalanceToCoverFees Fees Portfolio
    | OtherError String
    deriving (Generic, NFData)

instance Show MarketError where

    show = \case
        InsufficientBalanceForTransfer someAmount portfolio
            -> "insufficient balance for transfer: " ++ show someAmount
            ++ "; portfolio: " ++ show portfolio
        InsufficientBalanceToCoverFees fees portfolio
            -> "insufficient balance to cover fees: " ++ show fees
            ++ "; portfolio: " ++ show portfolio
        OtherError s -> s

type InitEffects c =
    [ Input Prices
    , Input (IConfig c)
    ]

type ExecuteEffects c s =
    [ Market
    , Time
    , Input Fees
    , Input Prices
    , Input Portfolio
    , Input (IConfig c)
    , State (IState s)
    , Error MarketError
    ]

type AggregateVisitor self agg
    = self -> StaticMap Asset agg -> agg

type SelfVisitor self
     = forall c s
     . Instrument c s
    => Prices -> Portfolio -> c -> s -> self

type Visitor self agg
     = AggregateVisitor self agg
    -> SelfVisitor self
    -> agg

class Truncatable s => Instrument c s | s -> c, c -> s where
    initState :: Members (InitEffects c) r => Sem r s
    initAllocation :: Members (InitEffects c) r => Sem r Distribution
    execute :: Members (ExecuteEffects c s) r => Sem r ()
    visit :: Prices -> Portfolio -> c -> s -> Visitor self agg

visit'
    :: forall self agg c s
     . Instrument c s
    => AggregateVisitor self agg
    -> SelfVisitor self
    -> Prices
    -> Portfolio
    -> c
    -> s
    -> agg
visit' visitAgg visitSelf prices portfolio config state
    = visit prices portfolio config state visitAgg visitSelf

runInstrument
    :: forall c s r a
     . (Instrument c s, Member (Input Prices) r)
    => c -> Sem (State (IState s) ': Input (IConfig c) ': r) a -> Sem r (s, a)
runInstrument config monad = do
    state <- runInputConst (IConfig config) initState
    runInstrument' config state monad

runInstrument'
    :: forall c s r a
     . (Instrument c s, Member (Input Prices) r)
    => c -> s -> Sem (State (IState s) ': Input (IConfig c) ': r) a
    -> Sem r (s, a)
runInstrument' config state monad = runInputConst (IConfig config) do
    (IState state', x) <- runState (IState state) monad
    return (state', x)

data SomeInstrumentConfig = SomeInstrumentConfig
    { someInitState :: Prices -> SomeInstrumentState
    , someInitAllocation :: Prices -> Distribution
    , someShow :: String
    }

data SomeInstrumentState = SomeInstrumentState
    { someExecute
        :: forall r
        .  Members
            ( ExecuteEffects
                SomeInstrumentConfig
                SomeInstrumentState
            ) r
        => Sem r ()
    , someVisit
        :: forall self agg
         . Prices
        -> Portfolio
        -> Visitor self agg
    , someTruncateTo
        :: forall res
         . HasResolution res
        => res
        -> SomeInstrumentState
    }

instance Truncatable SomeInstrumentState where
    truncateTo = flip someTruncateTo

instance Instrument SomeInstrumentConfig SomeInstrumentState where

    initState = do
        IConfig config <- input
        prices <- input
        return $ someInitState config prices

    initAllocation = do
        IConfig config <- input
        prices <- input
        return $ someInitAllocation config prices

    execute = do
        IState state <- State.get @(IState SomeInstrumentState)
        someExecute state

    visit prices portfolio _ state = someVisit state prices portfolio

instance Show SomeInstrumentConfig where
    show = someShow

someInstrumentConfig
    :: forall c s
    .  (Instrument c s, Show c)
    => c -> SomeInstrumentConfig
someInstrumentConfig config = SomeInstrumentConfig initSt initAlloc shw where
    initSt prices
        = someInstrumentState config
        $ run $ runInputConst prices $ runInputConst (IConfig config)
        $ initState
    initAlloc prices
        = run $ runInputConst prices $ runInputConst (IConfig config)
        $ initAllocation @c
    shw = show config

someInstrumentState
    :: forall c s
    .  Instrument c s
    => c -> s -> SomeInstrumentState
someInstrumentState config state = SomeInstrumentState exec vis trunc where
    exec
        :: forall r
        .  Members
            ( ExecuteEffects
                SomeInstrumentConfig
                SomeInstrumentState
            )
            r
        => Sem r ()
    exec = do
        IState state'
           <- runInputConst (IConfig config) $ execState (IState state)
            $ execute @c @s
        put @(IState SomeInstrumentState)
            $ IState $ someInstrumentState config state'

    vis :: Prices -> Portfolio -> Visitor self agg
    vis prices portfolio = visit prices portfolio config state

    trunc :: forall res. HasResolution res => res -> SomeInstrumentState
    trunc res = someInstrumentState config $ truncateTo res state
