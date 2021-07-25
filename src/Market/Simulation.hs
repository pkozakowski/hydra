{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Market.Simulation where

import Data.List.NonEmpty
import Data.Maybe
import Data.Record.Hom as HR
import Data.Time
import Data.Traversable
import Market as Market
import Market.Types
import Numeric.Algebra
import Numeric.Delta
import Numeric.Kappa
import Polysemy
import Polysemy.Error
import Polysemy.Reader
import Polysemy.State as State
import Prelude hiding (negate, pi)

runMarketSimulation
    :: forall assets r a
     . Members '[Reader (Prices assets), Error String] r
    => UTCTime
    -> Portfolio assets
    -> Sem (Market assets ': Reader (Portfolio assets) ': r) a
    -> Sem r (Portfolio assets, a)
runMarketSimulation time initPortfolio monad
    = runState initPortfolio $ marketReaderToState time monad

backtest
    :: forall assets c s r
     . (Instrument assets c s, Member (Error String) r)
    => TimeSeries (Prices assets)
    -> Portfolio assets
    -> c
    -> Sem r (Portfolio assets)
backtest priceSeries initPortfolio config
    = execState initPortfolio
    $ runReader initPrices
    $ runInstrument @assets config
    $ forM restOfPrices \(time, prices)
       -> runReader prices
        $ subsume @(State (Portfolio assets))
        $ marketReaderToState time
        $ execute @assets @c @s
    where
        TimeSeries ((_, initPrices) :| restOfPrices) = priceSeries

marketReaderToState
    :: forall assets r a
     . Members [Reader (Prices assets), Error String] r
    => UTCTime
    -> Sem (Market assets ': Reader (Portfolio assets) ': r) a
    -> Sem (State (Portfolio assets) : r) a
marketReaderToState time monad =
    let monad' :: Sem
             ( Market assets
            ': Reader (Portfolio assets)
            ': State (Portfolio assets)
            ': r
             )
             a
        monad' = insertAt @2 monad
    in
        readerToState $ marketToState time monad'

marketToState
    :: forall assets r a
     . Members
        [State (Portfolio assets), Reader (Prices assets), Error String] r
    => UTCTime
    -> Sem (Market assets ': r) a
    -> Sem r a
marketToState time = interpret \case
    Trade from to orderAmount -> do
        portfolio :: Portfolio assets <- State.get
        let fromBefore = HR.getIn from portfolio
            fromDelta
                = zero `delta` absoluteAmount fromBefore orderAmount
        fromAfter <- case fromBefore `sigma` fromDelta of
            Just amount -> return amount
            Nothing     -> throw $ "insufficient balance: " ++ show from
        if from == to then return () else do
            prices <- ask @(Prices assets)
            let fromPrice = HR.getIn from prices
                toPrice   = HR.getIn to prices
                toDelta   = negate $ fromJust
                    $ fromDelta `pi` fromPrice `kappa'` toPrice
                toAfter   = fromJust
                    $ HR.getIn to portfolio `sigma` toDelta
            put $ setIn from fromAfter $ setIn to toAfter portfolio
    GetTime -> return time

readerToState
    :: forall s r a
     . Member (State s) r
    => Sem (Reader s ': r) a
    -> Sem r a
readerToState monad = do
    state <- State.get
    runReader state monad
