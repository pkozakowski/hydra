{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}

module Market.Simulation where

import Control.Monad
import Data.List.NonEmpty
import Data.Maybe
import qualified Data.Record.Hom as HR
import Data.Time
import Data.Traversable
import Market as Market
import Market.Time
import Market.Types
import Numeric.Algebra
import Numeric.Delta
import Numeric.Kappa
import Numeric.Precision
import Numeric.Truncatable
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Internal
import Polysemy.State as State
import Prelude hiding (negate, pi, truncate)

runMarketSimulation
    :: forall assets r a
     . Members '[Input (Prices assets), Error String] r
    => Portfolio assets
    -> Sem (Market assets ': Input (Portfolio assets) ': r) a
    -> Sem r (Portfolio assets, a)
runMarketSimulation initPortfolio monad
    = runState initPortfolio
    $ marketInputToState monad

type BacktestEffects assets c s r =
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
     .  ( HR.Labels assets
        , Instrument assets c s
        , Members [Precision, Error String] r
        )
    => TimeSeries (Prices assets)
    -> Portfolio assets
    -> c
    -> OnStep assets c s r
    -> Sem r (Portfolio assets)
backtest priceSeries initPortfolio config onStep
    = backtest' priceSeries initPortfolio config (*> onStep)

type OnStep' assets c s r
    =  Sem (BacktestEffects assets c s r) ()
    -> Sem (BacktestEffects assets c s r) ()

-- | Advanced backtesting function, allowing to intercept the executed actions
-- at each timestep and run arbitrary effects.
backtest'
    :: forall assets c s r
     .  ( HR.Labels assets
        , Instrument assets c s
        , Members [Precision, Error String] r
        )
    => TimeSeries (Prices assets)
    -> Portfolio assets
    -> c
    -> OnStep' assets c s r
    -> Sem r (Portfolio assets)
backtest' priceSeries initPortfolio config onStep'
    = execState initPortfolio
    $ runInputConst initPrices
    $ runInstrument @assets config
    $ forM restOfPrices \(time, prices)
       -> runTimeConst time
        $ runInputConst prices
        $ (*> truncateState @(Portfolio assets))
        $ (*> truncateState @(IState s))
        $ subsume @(State (Portfolio assets))
        $ marketInputToState
        $ onStep'
        $ execute @assets @c @s
    where
        TimeSeries ((_, initPrices) :| restOfPrices) = priceSeries

        truncateState
            :: forall s r
             .  ( Truncatable s
                , Members [State s, Precision] r
                )
            => Sem r ()
        truncateState = do
            state <- get @s
            state' <- truncate state
            put state'

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
            put $ HR.setIn from fromAfter $ HR.setIn to toAfter portfolio

inputToState
    :: forall s r a
     . Member (State s) r
    => Sem (Input s ': r) a
    -> Sem r a
inputToState = runInputSem $ State.get @s @r
