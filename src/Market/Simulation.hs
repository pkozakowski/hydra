{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Market.Simulation where

import Data.List.NonEmpty
import Data.Maybe
import Data.Record.Hom as HR
import Data.Time
import Data.Traversable
import Market as Market
import Market.Time
import Market.Types
import Numeric.Algebra
import Numeric.Delta
import Numeric.Kappa
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Internal
import Polysemy.State as State
import Prelude hiding (negate, pi)

runMarketSimulation
    :: forall assets r a
     . Members '[Input (Prices assets), Error String] r
    => Portfolio assets
    -> Sem (Market assets ': Input (Portfolio assets) ': r) a
    -> Sem r (Portfolio assets, a)
runMarketSimulation initPortfolio monad
    = runState initPortfolio
    $ marketInputToState monad

type BacktestEffects assets c s (r :: EffectRow) =
    ( Market assets
    : Input (Portfolio assets)
    : Input (Prices assets)
    : Time
    : State (IState s)
    : Input (IConfig c)
    : Input (Prices assets)
    : State (Portfolio assets)
    : r
    )

type OnStep assets c s r = Sem (BacktestEffects assets c s r) ()

-- | Basic backtesting function, allowing to run arbitrary effects after each
-- timestep.
backtest
    :: forall assets c s r
     . (Instrument assets c s, Member (Error String) r)
    => OnStep assets c s r
    -> TimeSeries (Prices assets)
    -> Portfolio assets
    -> c
    -> Sem r (Portfolio assets)
backtest onStep = backtest' (*> onStep)

type OnStep' assets c s r
    =  Sem (BacktestEffects assets c s r) ()
    -> Sem (BacktestEffects assets c s r) ()

-- | Advanced backtesting function, allowing to intercept the executed actions
-- at each timestep and run arbitrary effects.
backtest'
    :: forall assets c s r
     . (Instrument assets c s, Member (Error String) r)
    => OnStep' assets c s r
    -> TimeSeries (Prices assets)
    -> Portfolio assets
    -> c
    -> Sem r (Portfolio assets)
backtest' onStep' priceSeries initPortfolio config
    = execState initPortfolio
    $ runInputConst initPrices
    $ runInstrument @assets config
    $ forM restOfPrices \(time, prices)
       -> runTimeConst time
        $ runInputConst prices
        $ subsume @(State (Portfolio assets))
        $ marketInputToState
        $ onStep'
        $ execute @assets @c @s
    where
        TimeSeries ((_, initPrices) :| restOfPrices) = priceSeries

marketInputToState
    :: forall assets r a
     . Members [Input (Prices assets), Error String] r
    => Sem (Market assets ': Input (Portfolio assets) ': r) a
    -> Sem (State (Portfolio assets) : r) a
marketInputToState monad =
    let monad' :: Sem
             ( Market assets
            ': Input (Portfolio assets)
            ': State (Portfolio assets)
            ': r
             )
             a
        monad' = insertAt @2 monad
    in
        inputToState $ marketToState monad'

marketToState
    :: forall assets r a
     . Members
        [State (Portfolio assets), Input (Prices assets), Error String] r
    => Sem (Market assets ': r) a
    -> Sem r a
marketToState = interpret \case
    Trade from to orderAmount -> do
        portfolio :: Portfolio assets <- State.get
        let fromBefore = HR.getIn from portfolio
            fromDelta
                = zero `delta` absoluteAmount fromBefore orderAmount
        fromAfter <- case fromBefore `sigma` fromDelta of
            Just amount -> return amount
            Nothing     -> throw $ "insufficient balance: " ++ show from
        if from == to then return () else do
            prices <- input @(Prices assets)
            let fromPrice = HR.getIn from prices
                toPrice   = HR.getIn to prices
                toDelta   = negate $ fromJust
                    $ fromDelta `pi` fromPrice `kappa'` toPrice
                toAfter   = fromJust
                    $ HR.getIn to portfolio `sigma` toDelta
            put $ setIn from fromAfter $ setIn to toAfter portfolio

inputToState
    :: forall s r a
     . Member (State s) r
    => Sem (Input s ': r) a
    -> Sem r a
inputToState = runInputSem $ State.get @s @r
