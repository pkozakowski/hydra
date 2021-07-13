{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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

data Hold (held :: Symbol) = Hold

instance (Has held assets, KnownSymbol held, Labels assets)
    => Instrument assets (Hold held) (Hold held) where

    initState _ _ = Hold
    initAllocation _ _ = onePoint $ labelIn @held
    -- TODO: allocation -> trades
    execute prices portfolio = return ()

data BalanceConfig assets instrs = BalanceConfig
    { configs :: HomRec instrs (SomeConfig assets)
    , target :: Distribution instrs
    , tolerance :: Scalar
    , updateEvery :: NominalDiffTime
    }

data Balance assets instrs = Balance
    { instruments :: HomRec instrs (SomeInstrument assets)
    , allocations :: HomRec instrs (Distribution assets)
    , lastUpdateTime :: UTCTime
    }

instance (Labels assets, Labels instrs)
    => Instrument
        assets (BalanceConfig assets instrs) (Balance assets instrs) where

    initState prices config = Balance
        { instruments = initState prices <$> configs config
        , allocations = initAllocation prices <$> configs config
        , lastUpdateTime = UTCTime (ModifiedJulianDay 0) 0
        }

    initAllocation prices config
        = redistribute (target config)
        $ initAllocation prices <$> configs config

    execute
        :: forall r
        .  Members
            ( Effects
                assets (BalanceConfig assets instrs) (Balance assets instrs)
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
                    @_ @(SomeConfig assets) @(SomeInstrument assets) prices
                executions
                    :: HomRec
                        instrs (Sem (Market assets ': r) (SomeInstrument assets))
                executions
                    = runReader <$> configs config <*>
                    ( execState <$> instruments state <*> (exec <$> portfolios) )
            portfoliosAndInstruments'
                <- sequence
                $  runMarketSimulation time prices <$> portfolios <*> executions
            let portfolios' = fst <$> portfoliosAndInstruments'
                instruments' = snd <$> portfoliosAndInstruments'
                allocations' = valueAlloc <$> portfolios'
            -- 3. Make the balancing trades between the old and new global
            -- portfolios.
            let portfolio' = foldl (+) zero portfolios'
                transfers = balancingTransfers
                    (tolerance config)
                    (valueAlloc portfolio)
                    (valueAlloc portfolio')
            sequence $ transferToTrade portfolio' <$> transfers
            -- 4. Update the state.
            put Balance
                { instruments = instruments'
                , allocations = allocations'
                , lastUpdateTime = time
                }
            where
                idealPortfolio value allocation
                    = fromJust $ value `unnormalize` allocation `kappa'` prices
                valueAlloc = fromJust . valueAllocation prices
                transferToTrade portfolio (ShareTransfer from to (Share shr))
                    = trade from to $ Absolute $ shr .* amount where
                        amount
                            = fromJust
                            $ totalValue prices portfolio
                                `kappa'` getIn from prices
