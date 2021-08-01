{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
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
import GHC.Stack
import GHC.TypeLits
import Market
import Market.Ops
import Market.Simulation
import Numeric.Algebra hiding ((>))
import Numeric.Kappa
import Numeric.Normalizable
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.State as State
import Prelude hiding ((+), pi)

import Debug.Trace

data Hold (held :: Symbol) = Hold (Proxy held)

instance KnownSymbol held => Show (Hold held) where
    show _ = "Hold " ++ symbolVal (Proxy @held)

instance (Has held assets, KnownSymbol held, Labels assets)
    => Instrument assets (Hold held) (Hold held) where

    initState = return $ Hold Proxy

    initAllocation = return $ onePoint $ labelIn @held

    execute = do
        prices <- input @(Prices assets)
        portfolio <- input @(Portfolio assets)
        allocationToTrades zero prices portfolio $ onePoint $ labelIn @held

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

    initState = do
        IConfig config <- input @(IConfig (BalanceConfig assets instrs))
        prices <- input @(Prices assets)
        states <- multiplexConfig (configs config)
            $ initState @assets @(SomeInstrumentConfig assets)
        allocations <- multiplexConfig (configs config)
            $ initAllocation @assets @(SomeInstrumentConfig assets)
        return BalanceState
            { states = states
            , allocations = allocations
            , lastUpdateTime = UTCTime (ModifiedJulianDay 0) 0
            }

    initAllocation = do
        IConfig config <- input @(IConfig (BalanceConfig assets instrs))
        allocations <- multiplexConfig (configs config)
            $ initAllocation @assets @(SomeInstrumentConfig assets)
        return $ redistribute (target config) allocations

    execute
        :: forall r
        .  Members
            ( InstrumentEffects
                assets (BalanceConfig assets instrs) (BalanceState assets instrs)
            ) r
        => Sem r ()
    execute = do
        -- 0. Check if we have any money and if enough time has passed since the
        -- last update.
        prices <- input @(Prices assets)
        portfolio <- input @(Portfolio assets)
        time <- getTime @assets
        IConfig config <- input @(IConfig (BalanceConfig assets instrs))
        IState state <- State.get
        when (shouldUpdate prices portfolio time config state) do
            -- 1. Compute the ideal per-instrument portfolios according to the
            -- value allocations.
            let Distribution targetShares = target config
                Values targetValues
                    = totalValue prices portfolio `unnormalize` target config
                portfolios
                    = idealPortfolio prices
                        <$> targetValues
                        <*> allocations state
            -- 2. Execute the per-instrument trades in simulated markets to get
            -- new portfolios.
            let exec = execute
                    @assets
                    @(SomeInstrumentConfig assets)
                    @(SomeInstrumentState assets)
                executions
                    = fmap (fmap fst)
                    $ runInstrument' @assets
                        <$> configs config
                        <*> states state
                        <*> pure exec
            portfoliosAndInstruments'
                <- sequence
                $  runMarketSimulation time <$> portfolios <*> executions
            let portfolios' = fst <$> portfoliosAndInstruments'
                states' = snd <$> portfoliosAndInstruments'
                allocations'
                    = valueAllocOr prices <$> portfolios' <*> allocations state
            -- 3. Make the balancing trades between the old and new global
            -- portfolios.
            let portfolio' = foldl (+) zero portfolios'
                allocation' = valueAlloc prices portfolio'
            allocationToTrades (tolerance config) prices portfolio allocation'
            -- 4. Update the state.
            put $ IState $ BalanceState
                { states = states'
                , allocations = allocations'
                , lastUpdateTime = time
                }
            where
                shouldUpdate prices portfolio time config state
                    =   totalValue prices portfolio > zero
                    &&  diff > updateEvery config where
                        diff = time `diffUTCTime` lastUpdateTime state

                idealPortfolio prices value allocation
                    = fromJust $ value `unnormalize` allocation `kappa'` prices

                valueAlloc
                    :: HasCallStack
                    => Prices assets -> Portfolio assets -> Distribution assets
                valueAlloc prices = fromJust . valueAllocation prices

                valueAllocOr prices portfolio allocation
                    = fromMaybe allocation $ valueAllocation prices portfolio

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

multiplexConfig
    :: Traversable f
    => f c -> Sem (Input (IConfig c) : r) a -> Sem r (f a)
multiplexConfig configs monad
    = sequence $ flip runInputConst monad <$> IConfig <$> configs
