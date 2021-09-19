{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}

module Market.Simulation where

import Control.Monad
import Data.List.NonEmpty
import Data.Maybe
import qualified Data.Record.Hom as HR
import Data.Time
import Data.Traversable
import Market
import Market.Ops
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
import Prelude hiding ((-), negate, pi, truncate)

runMarketSimulation
    :: forall assets r a
     .  ( HR.Labels assets
        , Members
            [ Input (Prices assets)
            , Input (Fees assets)
            , Error (MarketError assets)
            ] r
        )
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
    : Input (Fees assets)
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
        , Members [Precision, Error (MarketError assets)] r
        )
    => Fees assets
    -> TimeSeries (Prices assets)
    -> Portfolio assets
    -> c
    -> OnStep assets c s r
    -> Sem r (Portfolio assets)
backtest fees priceSeries initPortfolio config onStep
    = backtest' fees priceSeries initPortfolio config (*> onStep)

type OnStep' assets c s r
    =  Sem (BacktestEffects assets c s r) ()
    -> Sem (BacktestEffects assets c s r) ()

-- | Advanced backtesting function, allowing to intercept the executed actions
-- at each timestep and run arbitrary effects.
backtest'
    :: forall assets c s r
     .  ( HR.Labels assets
        , Instrument assets c s
        , Members [Precision, Error (MarketError assets)] r
        )
    => Fees assets
    -> TimeSeries (Prices assets)
    -> Portfolio assets
    -> c
    -> OnStep' assets c s r
    -> Sem r (Portfolio assets)
backtest' fees priceSeries initPortfolio config onStep'
    = execState initPortfolio
    $ runInputConst initPrices
    $ runInstrument @assets config
    $ forM restOfPrices \(time, prices)
       -> runTimeConst time
        $ runInputConst fees
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
     .  ( HR.Labels assets
        , Members
            [ Input (Prices assets)
            , Input (Fees assets)
            , Error (MarketError assets)
            ] r
        )
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
     .  ( HR.Labels assets
        , Members
            [ State (Portfolio assets)
            , Input (Prices assets)
            , Input (Fees assets)
            , Error (MarketError assets)
            ] r
        )
    => Sem (Market assets ': r) a
    -> Sem r a
marketToState = interpret \case
    Trade from to orderAmount -> do
        when (from == to)
            $ throw @(MarketError assets)
            $ OtherError $ "trying to trade for the same token: " ++ show to

        portfolioBefore :: Portfolio assets <- State.get
        let fromBefore = HR.getIn from portfolioBefore
            fromAmountBefore = absoluteAmount fromBefore orderAmount
        fees <- input @(Fees assets)
        (feeDelta, fromAmountAfterFees)
            <- case applyFees fees (from, fromAmountBefore) of
                Just x -> return x
                Nothing -> throw @(MarketError assets)
                    $ OtherError
                    $ "trade amount less than the fixed fee: "
                   ++ show (fromJust $ fixed fees)
        portfolioAfterFees <- case portfolioBefore `sigma` feeDelta of
            Just portfolio -> return $ portfolio
            Nothing -> throw $ InsufficientBalanceToCoverFees fees

        prices <- input @(Prices assets)
        let fromPrice = HR.getIn from prices
            toPrice   = HR.getIn to prices
            toAmount  = fromJust
                $ fromAmountAfterFees `pi` fromPrice `kappa'` toPrice
            totalDelta
                = transfer (to, toAmount)
                - transfer (from, fromAmountAfterFees)
        case portfolioAfterFees `sigma` totalDelta of
            Just portfolioAfterTrade -> put portfolioAfterTrade
            Nothing -> throw
                $ InsufficientBalanceForTransfer (from, fromAmountAfterFees)

inputToState
    :: forall s r a
     . Member (State s) r
    => Sem (Input s ': r) a
    -> Sem r a
inputToState = runInputSem $ State.get @s @r
