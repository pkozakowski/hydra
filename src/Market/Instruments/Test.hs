{-# LANGUAGE AllowAmbiguousTypes #-}

module Market.Instruments.Test where

import Control.Monad
import Data.Either
import Data.List.NonEmpty
import Data.Map.Sparse
import Data.Time
import Market
import Market.Ops
import Market.Simulation
import Market.Time
import Market.Types
import Numeric.Algebra hiding ((>))
import Numeric.Precision
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Output as Output
import Polysemy.State as State
import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Instances.Time
import Test.QuickCheck.Property

zeroFees :: Fees
zeroFees = Fees
    { fixed = Nothing
    , variable = zero
    }

instrumentLaws
    :: forall c s
     . (Show c, Instrument c s)
    => Gen c -> Gen c -> Laws
instrumentLaws arbitraryApproxConfig arbitraryExactConfig
    = Laws "Instrument"
        [   ( "Idempotence (Instant)"
            , instApprox $ idempotence @c @s )
        ,   ( "Idempotence (Continuous)"
            , contApprox $ idempotence @c @s )
        ,   ( "Efficiency (Instant)"
            , instApprox $ efficiency @c @s )
        ,   ( "Efficiency (Continuous)"
            , contApprox $ efficiency @c @s )
        ,   ( "Allocation Agreement (Instant)"
            , instExact $ allocationAgreement @c @s )
        ,   ( "Allocation Agreement (Continuous)"
            , contExact $ allocationAgreement @c @s )
        ] where
            instApprox = instrumentPropertyInstant arbitraryApproxConfig
            contApprox = instrumentPropertyContinuous arbitraryApproxConfig
            instExact = instrumentPropertyInstant arbitraryExactConfig
            contExact = instrumentPropertyContinuous arbitraryExactConfig

idempotence
    :: forall c s r
     . ( Instrument c s
       , Members (ExecuteEffects c s) r
       )
    => Sem r Property
idempotence = whenNotBroke @c @s do
    (portfolio', portfolio'') <- snd <$> runExecuteM @c do
        execute @c @s
        portfolio' <- input @Portfolio
        execute @c @s
        portfolio'' <- input @Portfolio
        return (portfolio', portfolio'')
    return $ portfolio' === portfolio''

data Sign = Plus | Zero | Minus deriving Eq

instance Default Sign where
    def = Zero

type RunExecuteEffects c s =
    [ State (IState s)
    , Input (IConfig c)
    , Time
    , Market
    , Input Portfolio
    , Input Prices
    , Input Fees
    , Error MarketError
    ]

efficiency
    :: forall c s r
     . ( Instrument c s
       , Members (ExecuteEffects c s) r
       )
    => Sem r Property
efficiency = whenNotBroke @c @s $ runEfficiencyTestM (execute @c @s) where
    runEfficiencyTestM
        :: Sem (RunExecuteEffects c s) ()
        -> Sem r Property
    runEfficiencyTestM monad = do
        IConfig config <- input
        IState state <- State.get
        time <- now
        portfolio <- input @Portfolio
        subsume_
            $ fmap eitherToProperty
            $ runError
            $ runState (const Zero `remap` portfolio :: SparseMap Asset Sign)
            $ reinterpret2 \case
                Trade from to orderAmount -> do
                    signs <- State.get @(SparseMap Asset Sign)
                    when (absoluteAmount (portfolio ! from) orderAmount == zero)
                        $ throw
                        $ counterexample "trade with zero amount" False
                    assertSign from Plus signs
                    assertSign to Minus signs
                    put $ set from Minus $ set to Plus $ signs
            $ runTimeConst time
            $ runInstrument' config state
            $ monad
        where
            assertSign
                :: Member (Error Property) r'
                => Asset -> Sign -> SparseMap Asset Sign
                -> Sem r' ()
            assertSign asset sign signs
                = when (signs ! asset == sign)
                $ throw
                $ counterexample
                    ("incoherent trades for " ++ show asset)
                    False
            eitherToProperty = \case
                Right _   -> property succeeded
                Left prop -> prop
            isZero = \case
                Absolute x         -> x == zero
                Relative (Share x) -> x == zero

allocationAgreement
    :: forall c s r
     . ( Instrument c s
       , Members (ExecuteEffects c s) r
       )
    => Sem r Property
allocationAgreement = whenNotBroke @c @s do
    IConfig config <- input @(IConfig c)
    prices <- input
    portfolio <- fmap fst $ runExecuteM @c $ execute @c @s
    let initAlloc = runInit prices config $ initAllocation @c
    return $ valueAllocation prices portfolio === Just initAlloc

whenNotBroke
    :: forall c s r
     . Members (ExecuteEffects c s) r
    => Sem r Property
    -> Sem r Property
whenNotBroke monad = do
    prices <- input
    portfolio <- input
    if totalValue prices portfolio > zero then
        monad
    else
        return discard

runInit
    :: Instrument c s
    => Prices -> c -> Sem (InitEffects c) a -> a
runInit prices config
    = run . runInputConst (IConfig config) . runInputConst prices

instrumentPropertyInstant
    :: forall c s
     . (Show c, Instrument c s)
    => Gen c
    -> Sem (RunExecuteEffects c s) Property
    -> Property
instrumentPropertyInstant arbitraryConfig monad
    = forAll arbitraryConfig prop where
        prop :: c -> UTCTime -> Prices -> Portfolio -> Property
        prop config time prices portfolio
            = snd $ fromRight undefined
            $ runInitExecute config time zeroFees prices portfolio monad

instrumentPropertyContinuous
    :: forall c s
     . (Show c, Instrument c s)
    => Gen c
    -> Sem (RunExecuteEffects c s) Property
    -> Property
instrumentPropertyContinuous arbitraryConfig monad
    = forAll ((,) <$> arbitraryConfig <*> resize 5 arbitrary)
    $ uncurry prop where
        prop :: c -> TimeSeries Prices -> Portfolio -> Property
        prop config priceSeries initPortfolio
            = (totalValue initPrices initPortfolio > zero ==>)
            $ conjoin
            $ fromRight undefined
            $ run
            $ runError @MarketError
            $ fmap fst
            $ runOutputList @Property
            $ runPrecisionExact
            $ backtest' @c @s zeroFees priceSeries initPortfolio config \exec -> do
                instantProp <- snd <$> runExecuteM monad
                Output.output instantProp
                exec
            where
                TimeSeries ((_, initPrices) :| _) = priceSeries

runInitExecute
    :: forall c s a
     . Instrument c s
    => c -> UTCTime -> Fees -> Prices -> Portfolio
    -> Sem (RunExecuteEffects c s) a
    -> Either MarketError (Portfolio, a)
runInitExecute config time fees prices
    = runExecute config state time fees prices where
        state = runInit prices config initState

runExecute
    :: forall c s a
     . Instrument c s
    => c -> s -> UTCTime -> Fees -> Prices -> Portfolio
    -> Sem (RunExecuteEffects c s) a
    -> Either MarketError (Portfolio, a)
runExecute config state time fees prices portfolio
    = run . runError
    . runInputConst fees
    . runInputConst prices
    . runMarketSimulation portfolio
    . runTimeConst time
    . fmap snd
    . runInstrument' config state

runExecuteM
    :: forall c s a r
     . (Instrument c s, Members (ExecuteEffects c s) r)
    => Sem (RunExecuteEffects c s) a
    -> Sem r (Portfolio, a)
runExecuteM monad = do
    IConfig config <- input
    IState state <- State.get
    time <- now
    fees <- input
    prices <- input
    portfolio <- input
    fromEither $ runExecute config state time fees prices portfolio monad