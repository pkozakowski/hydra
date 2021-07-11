{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Market.Simulation where

import Data.Maybe
import Data.Record.Hom as HR
import Data.Time
import Market
import Market.Types
import Numeric.Algebra
import Numeric.Delta
import Numeric.Kappa
import Polysemy
import Polysemy.Error as Error
import Polysemy.State as State
import Prelude hiding (negate, pi)

runMarketSimulation
    :: Member (Error String) r
    => UTCTime
    -> Prices assets
    -> Portfolio assets
    -> Sem (Market assets ': r) a
    -> Sem r (Portfolio assets, a)
runMarketSimulation time prices initPortfolio
    = runState initPortfolio . reinterpret \case
        Trade from to orderAmount -> do
            portfolio :: Portfolio assets <- State.get
            let fromBefore = HR.getIn from portfolio
                fromDelta  = zero `delta` absoluteAmount fromBefore orderAmount
            fromAfter <- case fromBefore `sigma` fromDelta of
                Just amount -> return amount
                Nothing     -> throw $ "insufficient balance: " ++ show from
            if from == to then return () else
                let fromPrice = HR.getIn from prices
                    toPrice   = HR.getIn to prices
                    toDelta   = negate $ fromJust
                        $ fromDelta `pi` fromPrice `kappa'` toPrice
                    toAfter   = fromJust
                        $ HR.getIn to portfolio `sigma` toDelta
                in
                    put $ setIn from fromAfter $ setIn to toAfter portfolio
        GetTime -> return time
