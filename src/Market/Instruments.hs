{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
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
import Market.Time
import Numeric.Algebra hiding ((>))
import Numeric.Kappa
import Numeric.Normalizable
import Numeric.Truncatable
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.State as State
import Prelude hiding ((+), pi)

data Hold (held :: Symbol) = Hold (Proxy held)

instance KnownSymbol held => Show (Hold held) where
    show _ = "Hold " ++ symbolVal (Proxy @held)

instance Truncatable (Hold held) where
    truncateTo _ = id

instance (Has held assets, KnownSymbol held, Labels assets)
    => Instrument assets (Hold held) (Hold held) where

    initState = return $ Hold Proxy

    initAllocation = return $ onePoint $ labelIn @held

    execute = do
        prices <- input @(Prices assets)
        portfolio <- input @(Portfolio assets)
        allocationToTrades zero prices portfolio $ onePoint $ labelIn @held

    visit prices portfolio config state visitAgg visitSelf
        = visitAgg (visitSelf prices portfolio config state) Empty

data BalanceConfig assets sis = BalanceConfig
    { configs :: HomRec sis (SomeInstrumentConfig assets)
    , target :: Distribution sis
    , tolerance :: Scalar
    , updateEvery :: NominalDiffTime
    } deriving (Show)

data BalanceState assets sis = BalanceState
    { states :: HomRec sis (SomeInstrumentState assets)
    , allocations :: HomRec sis (Distribution assets)
    , lastUpdateTime :: UTCTime
    }

noConfigs :: forall assets. HomRec '[] (SomeInstrumentConfig assets)
noConfigs = Empty

(~&)
    :: forall i sis assets c s
     .  (NoDuplicateIn i sis
        , KnownSymbol i
        , Labels sis
        , Instrument assets c s
        , Show c
        )
    => i := c
    -> HomRec sis (SomeInstrumentConfig assets)
    -> HomRec (i : sis) (SomeInstrumentConfig assets)
instr := config ~& configs
    = instr := someInstrumentConfig @assets config :& configs

infixr 5 ~&

instance (Labels assets, Labels sis)
    => Truncatable (BalanceState assets sis) where

    truncateTo res
        = BalanceState
            <$> truncateTo res . states
            <*> truncateTo res . allocations
            <*> lastUpdateTime

instance (Labels assets, Labels sis)
    => Instrument
        assets
        (BalanceConfig assets sis)
        (BalanceState assets sis) where

    initState = do
        IConfig config <- input @(IConfig (BalanceConfig assets sis))
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
        IConfig config <- input @(IConfig (BalanceConfig assets sis))
        allocations <- multiplexConfig (configs config)
            $ initAllocation @assets @(SomeInstrumentConfig assets)
        return $ redistribute (target config) allocations

    execute
        :: forall r
         . Members
            ( ExecuteEffects
                assets
                (BalanceConfig assets sis)
                (BalanceState assets sis)
            ) r
        => Sem r ()
    execute = do
        -- 0. Check if we have any money and if enough time has passed since the
        -- last update.
        prices <- input @(Prices assets)
        portfolio <- input @(Portfolio assets)
        time <- now
        IConfig config <- input @(IConfig (BalanceConfig assets sis))
        IState state <- State.get
        when (shouldUpdate prices portfolio time config state) do
            -- 1. Compute the ideal per-instrument portfolios according to the
            -- value allocations.
            let portfolios = distributePortfolio config state prices portfolio
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
                $  runMarketSimulation <$> portfolios <*> executions
            let portfolios' = fst <$> portfoliosAndInstruments'
                states' = snd <$> portfoliosAndInstruments'
                allocations'
                    = valueAllocOr prices <$> portfolios' <*> allocations state
            -- 3. Make balancing trades between the old and new global
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

                valueAlloc
                    :: HasCallStack
                    => Prices assets -> Portfolio assets -> Distribution assets
                valueAlloc prices = fromJust . valueAllocation prices

                valueAllocOr prices portfolio allocation
                    = fromMaybe allocation $ valueAllocation prices portfolio

    visit
        :: forall self agg
         . Prices assets
        -> Portfolio assets
        -> BalanceConfig assets sis
        -> BalanceState assets sis
        -> Visitor assets self agg
    visit prices portfolio config state visitAgg visitSelf
        = visitAgg (visitSelf prices portfolio config state)
        $ visit' @assets visitAgg visitSelf prices
            <$> distributePortfolio config state prices portfolio
            <*> configs config
            <*> states state

distributePortfolio
    :: (Labels assets, Labels sis)
    => BalanceConfig assets sis
    -> BalanceState assets sis
    -> Prices assets
    -> Portfolio assets
    -> HomRec sis (Portfolio assets)
distributePortfolio config state prices portfolio
    = idealPortfolio prices <$> targetValues <*> allocations state where
        idealPortfolio prices value allocation
            = fromJust $ value `unnormalize` allocation `kappa'` prices
        Distribution targetShares = target config
        Values targetValues
            = totalValue prices portfolio `unnormalize` target config

allocationToTrades
    :: forall assets r
     . (Labels assets, Members [Market assets, Error (MarketError assets)] r)
    => Scalar -> Prices assets -> Portfolio assets -> Distribution assets
    -> Sem r ()
allocationToTrades tolerance prices portfolio targetAlloc
    = when (value > zero) $ sequence_ $ transferToTrade value <$> transfers
    where
        value = totalValue prices portfolio
        transferToTrade value (ShareTransfer from to (Share shr))
            = catch @(MarketError assets)
                (trade from to $ Absolute $ shr .* amount)
                \case
                    -- Can't afford the fees => skip this trade - instruments
                    -- aren't supposed check that.
                    InsufficientBalanceToCoverFees _ -> return ()
                    -- Don't have enough money for the transfer => throw -
                    -- instruments shouldn't exceed the balance.
                    e -> throw e
            where
                amount = fromJust $ value `kappa'` getIn from prices
        transfers = balancingTransfers tolerance currentAlloc targetAlloc where
            currentAlloc = fromJust $ valueAllocation prices portfolio

multiplexConfig
    :: Traversable f
    => f c -> Sem (Input (IConfig c) : r) a -> Sem r (f a)
multiplexConfig configs monad
    = sequence $ flip runInputConst monad <$> IConfig <$> configs
