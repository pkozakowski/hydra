{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}

module Market.Simulation where

import Control.Monad
import Data.List.NonEmpty
import Data.Map.Class ((!))
import Data.Maybe
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
    :: forall r a
     .  ( Members
            [ Input Prices
            , Input Fees
            , Error MarketError
            ] r
        )
    => Portfolio
    -> Sem (Market ': Input Portfolio ': r) a
    -> Sem r (Portfolio, a)
runMarketSimulation initPortfolio monad
    = runState initPortfolio
    $ marketInputToState monad

type BacktestEffects c s r =
    ( Market
    : Input Portfolio
    : Input Prices
    : Input Fees
    : Time
    : State (IState s)
    : Input (IConfig c)
    : Input Prices
    : State Portfolio
    : r
    )

type OnStep c s r = Sem (BacktestEffects c s r) ()

-- | Basic backtesting function, allowing to run arbitrary effects after each
-- timestep.
backtest
    :: forall c s r
     .  ( Instrument c s
        , Members [Precision, Error MarketError] r
        )
    => Fees
    -> TimeSeries Prices
    -> Portfolio
    -> c
    -> OnStep c s r
    -> Sem r Portfolio
backtest fees priceSeries initPortfolio config onStep
    = backtest' fees priceSeries initPortfolio config (*> onStep)

type OnStep' c s r
    =  Sem (BacktestEffects c s r) ()
    -> Sem (BacktestEffects c s r) ()

-- | Advanced backtesting function, allowing to intercept the executed actions
-- at each timestep and run arbitrary effects.
backtest'
    :: forall c s r
     .  ( Instrument c s
        , Members [Precision, Error MarketError] r
        )
    => Fees
    -> TimeSeries Prices
    -> Portfolio
    -> c
    -> OnStep' c s r
    -> Sem r Portfolio
backtest' fees priceSeries initPortfolio config onStep'
    = execState initPortfolio
    $ runInputConst initPrices
    $ runInstrument config
    $ forM restOfPrices \(time, prices)
       -> runTimeConst time
        $ runInputConst fees
        $ runInputConst prices
        $ (*> truncateState @Portfolio)
        $ (*> truncateState @(IState s))
        $ subsume @(State Portfolio)
        $ marketInputToState
        $ onStep'
        $ execute @c @s
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
    :: forall r a
     .  ( Members
            [ Input Prices
            , Input Fees
            , Error MarketError
            ] r
        )
    => Sem (Market ': Input Portfolio ': r) a
    -> Sem (State Portfolio : r) a
marketInputToState monad =
    let monad' :: Sem
             ( Market
            ': Input Portfolio
            ': State Portfolio
            ': r
             )
             a
        monad' = insertAt @2 monad
    in
        inputToState $ marketToState monad'

marketToState
    :: forall r a
     .  ( Members
            [ State Portfolio
            , Input Prices
            , Input Fees
            , Error MarketError
            ] r
        )
    => Sem (Market ': r) a
    -> Sem r a
marketToState = interpret \case
    Trade from to orderAmount -> do
        when (from == to)
            $ throw @MarketError
            $ OtherError $ "trying to trade for the same token: " ++ show to

        portfolioBefore :: Portfolio <- State.get
        let fromBefore = portfolioBefore ! from
            fromAmountBefore = absoluteAmount fromBefore orderAmount
        fees <- input @Fees
        (feeDelta, fromAmountAfterFees)
            <- case applyFees fees (from, fromAmountBefore) of
                Just x -> return x
                Nothing -> throw @MarketError
                    $ OtherError
                    $ "trade amount less than the fixed fee: "
                   ++ show (fromJust $ fixed fees)
        portfolioAfterFees <- case portfolioBefore `sigma` feeDelta of
            Just portfolio -> return $ portfolio
            Nothing -> throw $ InsufficientBalanceToCoverFees fees

        prices <- input @Prices
        let fromPrice = prices ! from
            toPrice   = prices ! to
            toAmount  = fromJust
                $ fromPrice `pi` fromAmountAfterFees `kappa` toPrice
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
