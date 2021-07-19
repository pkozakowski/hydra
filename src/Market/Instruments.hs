{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Market.Instruments where

import Control.Monad
import Data.Maybe
import Data.Proxy
import Data.Record.Hom
import Data.Time
import GHC.TypeLits
import Market
import Market.Ops
import Market.Simulation
import Numeric.Algebra hiding ((>))
import Numeric.Kappa
import Numeric.Normalizable
import Polysemy
import Polysemy.Error
import Polysemy.Reader
import Polysemy.State as State
import Prelude hiding ((+), pi)

import Debug.Trace

data Hold (held :: Symbol) = Hold (Proxy held)

instance KnownSymbol held => Show (Hold held) where
    show _ = "Hold " ++ symbolVal (Proxy @held)

instance (Has held assets, KnownSymbol held, Labels assets)
    => Instrument assets (Hold held) (Hold held) where

    initState _ _ = Hold Proxy
    initAllocation _ _ = onePoint $ labelIn @held
    execute prices portfolio
        = allocationToTrades zero prices portfolio $ onePoint $ labelIn @held

data BalanceConfig assets instrs = BalanceConfig
    { configs :: HomRec instrs (SomeInstrumentConfig assets)
    , target :: Distribution instrs
    , tolerance :: Scalar
    , updateEvery :: NominalDiffTime
    } deriving (Show)

data BalanceState assets instrs = BalanceState
    { states :: HomRec instrs (SomeInstrumentState assets)
    , allocations :: HomRec instrs (Distribution assets)
    , lastUpdateTime :: UTCTime
    }

instance (Labels assets, Labels instrs)
    => Instrument
        assets (BalanceConfig assets instrs) (BalanceState assets instrs) where

    initState prices config = BalanceState
        { states = initState prices <$> configs config
        , allocations = initAllocation prices <$> configs config
        , lastUpdateTime = UTCTime (ModifiedJulianDay 0) 0
        }

    initAllocation prices config
        = redistribute (target config)
        $ initAllocation prices <$> configs config

    execute
        :: forall r
        .  Members
            ( InstrumentEffects
                assets (BalanceConfig assets instrs) (BalanceState assets instrs)
            ) r
        => Prices assets -> Portfolio assets -> Sem r ()
    execute prices portfolio = do
        -- 0. Check if enough time has passed since the last update.
        time <- getTime @assets
        config :: BalanceConfig assets instrs <- ask
        state <- State.get
        when (time `diffUTCTime` lastUpdateTime state > updateEvery config) $ do
            -- 1. Compute the ideal per-instrument portfolios according to the
            -- value allocations.
            let Distribution targetShares = target config
                Values targetValues
                    = totalValue prices portfolio `unnormalize` target config
                portfolios = idealPortfolio <$> targetValues <*> allocations state
            -- 2. Execute the per-instrument trades in simulated markets to get
            -- new portfolios.
            let exec = execute
                    @_
                    @(SomeInstrumentConfig assets)
                    @(SomeInstrumentState assets)
                    prices
                executions
                    :: HomRec
                        instrs
                        (Sem (Market assets ': r) (SomeInstrumentState assets))
                executions
                    = runReader <$> configs config <*>
                    ( execState <$> states state <*> (exec <$> portfolios) )
            portfoliosAndInstruments'
                <- sequence
                $  runMarketSimulation time prices <$> portfolios <*> executions
            let portfolios' = fst <$> portfoliosAndInstruments'
                states' = snd <$> portfoliosAndInstruments'
                allocations' = valueAlloc <$> portfolios'
            -- 3. Make the balancing trades between the old and new global
            -- portfolios.
            let portfolio' = foldl (+) zero portfolios'
                allocation' = valueAlloc portfolio'
            allocationToTrades (tolerance config) prices portfolio allocation'
            -- 4. Update the state.
            put BalanceState
                { states = states'
                , allocations = allocations'
                , lastUpdateTime = time
                }
            where
                idealPortfolio value allocation
                    = fromJust $ value `unnormalize` allocation `kappa'` prices
                valueAlloc = fromJust . valueAllocation prices

allocationToTrades
    :: (Labels assets, Member (Market assets) r)
    => Scalar -> Prices assets -> Portfolio assets -> Distribution assets
    -> Sem r ()
allocationToTrades tolerance prices portfolio targetAlloc =
    when (value > zero) $ sequence_ $ transferToTrade value <$> transfers
    where
        value = totalValue prices portfolio
        transferToTrade value (ShareTransfer from to (Share shr))
            = trade from to $ Absolute $ shr .* amount where
                amount = fromJust $ value `kappa'` getIn from prices
        transfers = balancingTransfers tolerance currentAlloc targetAlloc where
            currentAlloc = fromJust $ valueAllocation prices portfolio
