{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Market.Instruments where

import Control.Monad
import Data.Functor.Apply
import Data.Map.Static
import Data.Maybe
import Data.Time
import GHC.Stack
import GHC.TypeLits
import Market
import Market.Ops
import Market.Simulation
import Market.Time
import Numeric.Algebra hiding ((>))
import Numeric.Kappa
import Numeric.Normed
import Numeric.Truncatable
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.State as State
import Prelude hiding ((+), pi)

data Hold = Hold { held :: Asset }
    deriving Show

instance Truncatable Hold where
    truncateTo _ = id

instance Instrument Hold Hold where

    initState = unIConfig <$> input

    initAllocation = onePoint . held . unIConfig <$> input

    execute = do
        IConfig (Hold asset) <- input
        allocationToTrades zero $ onePoint asset

    visit prices portfolio config state visitAgg visitSelf
        = visitAgg (visitSelf prices portfolio config state) empty

data BalanceConfig = BalanceConfig
    { configs :: StaticMap Asset SomeInstrumentConfig
    , target :: Distribution
    , tolerance :: Scalar
    , updateEvery :: NominalDiffTime
    } deriving (Show)

data BalanceState = BalanceState
    { states :: StaticMap Asset SomeInstrumentState
    , allocations :: StaticMap Asset Distribution
    , lastUpdateTime :: UTCTime
    }

instance Truncatable BalanceState where

    truncateTo res
        = BalanceState
            <$> truncateTo res . states
            <*> truncateTo res . allocations
            <*> lastUpdateTime

instance Instrument BalanceConfig BalanceState where

    initState = do
        IConfig config <- input @(IConfig (BalanceConfig))
        prices <- input @(Prices)
        states <- multiplexConfig (configs config)
            $ initState @(SomeInstrumentConfig)
        allocations <- multiplexConfig (configs config)
            $ initAllocation @(SomeInstrumentConfig)
        return BalanceState
            { states = states
            , allocations = allocations
            , lastUpdateTime = UTCTime (ModifiedJulianDay 0) 0
            }

    initAllocation = do
        IConfig config <- input @(IConfig (BalanceConfig))
        allocations <- multiplexConfig (configs config)
            $ initAllocation @(SomeInstrumentConfig)
        return $ redistribute (target config) allocations

    execute
        :: forall r
         . Members (ExecuteEffects BalanceConfig BalanceState) r
        => Sem r ()
    execute = do
        -- 0. Check if we have any money and if enough time has passed since the
        -- last update.
        prices <- input @Prices
        portfolio <- input @Portfolio
        time <- now
        IConfig config <- input @(IConfig (BalanceConfig))
        IState state <- State.get
        when (shouldUpdate prices portfolio time config state) do
            -- 1. Compute the ideal per-instrument portfolios according to the
            -- value allocations.
            let portfolios = distributePortfolio config state prices portfolio
            -- 2. Execute the per-instrument trades in simulated markets to get
            -- new portfolios.
            fees <- input @Fees @r
            let exec = execute @SomeInstrumentConfig @SomeInstrumentState
                executions
                    = fmap
                        ( fmap fst
                        -- Forward the real fees to the instruments.
                        . inject fees
                        )
                    $ (\c s -> runInstrument' c s exec)
                        <$> configs config
                        <.> states state
            portfoliosAndInstruments'
                -- Run the simulation with zero fees, so the instruments don't
                -- have to allocate funds for them.
               <- inject zeroFees
                $ sequence
                $ runMarketSimulation <$> portfolios <.> executions
            let portfolios' = fst <$> portfoliosAndInstruments'
                states' = snd <$> portfoliosAndInstruments'
                allocations'
                    = valueAllocOr prices <$> portfolios' <.> allocations state
            -- 3. Make balancing trades between the old and new global
            -- portfolios.
            let portfolio' = foldl (+) zero portfolios'
                allocation' = valueAlloc prices portfolio'
            allocationToTrades (tolerance config) allocation'
            -- 4. Update the state.
            put $ IState $ BalanceState
                { states = states'
                , allocations = allocations'
                , lastUpdateTime = time
                }
            where
                shouldUpdate prices portfolio time config state
                     = totalValue prices portfolio > zero
                    && diff > updateEvery config where
                        diff = time `diffUTCTime` lastUpdateTime state

                valueAlloc
                    :: HasCallStack
                    => Prices -> Portfolio -> Distribution
                valueAlloc prices = fromJust . valueAllocation prices

                valueAllocOr prices portfolio allocation
                    = fromMaybe allocation $ valueAllocation prices portfolio

                inject
                    :: forall i r a
                     . Member (Input i) r
                    => i -> Sem r a -> Sem r a
                inject inp = intercept @(Input i) \case
                    Input -> pure inp

    visit
        :: forall self agg
         . Prices
        -> Portfolio
        -> BalanceConfig
        -> BalanceState
        -> Visitor self agg
    visit prices portfolio config state visitAgg visitSelf
        = visitAgg (visitSelf prices portfolio config state)
        $ visit' visitAgg visitSelf prices
            <$> distributePortfolio config state prices portfolio
            <.> configs config
            <.> states state

distributePortfolio
    :: BalanceConfig
    -> BalanceState
    -> Prices
    -> Portfolio
    -> StaticMap Asset Portfolio
distributePortfolio config state prices portfolio
    = idealPortfolio prices <$> targetValues `reapplyOuter` allocations state
        where
            idealPortfolio prices value allocation
                = fromJust $ value `scale` allocation `kappa` prices
            Distribution targetShares = target config
            Values targetValues
                = totalValue prices portfolio `scale` target config

allocationToTrades
    :: forall r
     . Members
        [ Market
        , Input Portfolio
        , Input Prices
        , Input Fees
        , Error MarketError
        ] r
    => Scalar -> Distribution -> Sem r ()
allocationToTrades tolerance targetAlloc
    = do
        prices <- input @Prices
        portfolio <- input @Portfolio
        let value = totalValue prices portfolio
        when (value > zero) do
            let currentAlloc = fromJust $ valueAllocation prices portfolio
                transfers
                    = balancingTransfers tolerance currentAlloc targetAlloc
            sequence_ $ transferToTrade value <$> transfers
    where
        transferToTrade value (ShareTransfer from to shr) = do
            prices <- input @Prices @r
            portfolio' <- input @Portfolio
            let balance = portfolio' ! from
                amount = fromJust $ value `kappa` (prices ! from)
            -- Convert from share in the portfolio to share in the balance of
            -- a specific asset.
            orderAmount <- case amount `pi` shr `kappa` balance of
                Just shr' -> return
                    -- balance can be lower than amount `pi` shr
                    -- because of fixed fees, so we clip.
                    $ Relative $ min shr' $ Share one
                Nothing -> throw
                    $ InsufficientBalanceForTransfer
                        (from, amount) portfolio'

            when (not $ orderAmountIsZero orderAmount)
                $ trade from to orderAmount
                    `catch` \case
                        -- Can't afford the fees => skip this trade -
                        -- instruments don't have to check that.
                        InsufficientBalanceToCoverFees _ _ -> return ()
                        -- Don't have enough money for the transfer => throw -
                        -- instruments shouldn't exceed the balance.
                        e -> throw e

multiplexConfig
    :: Traversable f
    => f c -> Sem (Input (IConfig c) : r) a -> Sem r (f a)
multiplexConfig configs monad
    = sequence $ flip runInputConst monad <$> IConfig <$> configs
