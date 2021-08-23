{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}

module Market.Instruments.Test where

import Control.Monad
import Data.Either
import Data.List.NonEmpty
import Data.Record.Hom
import Data.Time
import Market
import Market.Ops
import Market.Simulation
import Market.Time
import Market.Types
import Market.Types.Test
import Numeric.Algebra hiding ((>))
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Output as Output
import Polysemy.State as State
import Test.QuickCheck
import Test.QuickCheck.Instances.Time
import Test.QuickCheck.Property
import Test.Tasty
import Test.Tasty.QuickCheck

testInstrumentLaws
    :: forall assets c s. (Labels assets, Show c, Instrument assets c s)
    => Gen c -> Gen c -> TestTree
testInstrumentLaws arbitraryApproxConfig arbitraryExactConfig
    = testGroup "Instrument"
        [ testInstantApprox "Idempotence (Instant)"
            $ idempotence @assets
        , testContinuousApprox "Idempotence (Continuous)"
            $ idempotence @assets
        , testInstantApprox "Efficiency (Instant)"
            $ efficiency @assets
        , testContinuousApprox "Efficiency (Continuous)"
            $ efficiency @assets
        , testInstantExact "Allocation Agreement (Instant)"
            $ allocationAgreement @assets
        , testContinuousExact "Allocation Agreement (Continuous)"
            $ allocationAgreement @assets
        ] where
            testInstantApprox
                = testInstrumentPropertyInstant arbitraryApproxConfig
            testContinuousApprox
                = testInstrumentPropertyContinuous arbitraryApproxConfig
            testInstantExact
                = testInstrumentPropertyInstant arbitraryExactConfig
            testContinuousExact
                = testInstrumentPropertyContinuous arbitraryExactConfig

idempotence
    :: forall assets c s r
     . ( Labels assets
       , Instrument assets c s
       , Members (ExecuteEffects assets c s) r
       )
    => Sem r Property
idempotence = whenNotBroke do
    (portfolio', portfolio'') <- snd <$> runExecuteM do
        execute
        portfolio' <- input @(Portfolio assets)
        execute
        portfolio'' <- input @(Portfolio assets)
        return (portfolio', portfolio'')
    return $ portfolio' === portfolio''

data Sign = Plus | Zero | Minus deriving Eq

type RunExecuteEffects assets c s =
    [ State (IState s)
    , Input (IConfig c)
    , Time
    , Market assets
    , Input (Portfolio assets)
    , Input (Prices assets)
    , Error String
    ]

efficiency
    :: forall assets c s r
     . ( Labels assets
       , Instrument assets c s
       , Members (ExecuteEffects assets c s) r
       )
    => Sem r Property
efficiency = whenNotBroke $ runEfficiencyTestM execute where
    runEfficiencyTestM
        :: Sem (RunExecuteEffects assets c s) ()
        -> Sem r Property
    runEfficiencyTestM monad = do
        IConfig config <- input
        IState state <- State.get
        time <- now
        prices <- input @(Prices assets)
        portfolio <- input @(Portfolio assets)
        subsume_
            $ fmap eitherToProperty
            $ runError
            $ runState (pure Zero :: HomRec assets Sign)
            $ reinterpret2 \case
                Trade from to orderAmount -> do
                    signs <- State.get @(HomRec assets Sign)
                    let fromAmount = getIn from portfolio
                    when (absoluteAmount fromAmount orderAmount == zero)
                        $ throw
                        $ counterexample "trade with zero amount" False
                    assertSign from Plus signs
                    assertSign to Minus signs
                    put $ setIn from Minus $ setIn to Plus $ signs
            $ runTimeConst time
            $ runInstrument' config state
            $ monad
        where
            assertSign
                :: Member (Error Property) r'
                => LabelIn assets -> Sign -> HomRec assets Sign
                -> Sem r' ()
            assertSign asset sign signs
                = when (getIn asset signs == sign)
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
    :: forall assets c s r
     . ( Labels assets
       , Instrument assets c s
       , Members (ExecuteEffects assets c s) r
       )
    => Sem r Property
allocationAgreement = whenNotBroke do
    IConfig config <- input
    prices <- input
    portfolio <- fst <$> runExecuteM execute
    let initAlloc = runInit prices config $ initAllocation
    return $ valueAllocation prices portfolio === Just initAlloc

whenNotBroke
    :: forall assets c s r
     . (Labels assets, Members (ExecuteEffects assets c s) r)
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
    :: Instrument assets c s
    => Prices assets -> c -> Sem (InitEffects assets c) a -> a
runInit prices config
    = run . runInputConst (IConfig config) . runInputConst prices

testInstrumentPropertyInstant
    :: forall assets c s
     . (Labels assets, Show c, Instrument assets c s)
    => Gen c -> String
    -> Sem (RunExecuteEffects assets c s) Property
    -> TestTree
testInstrumentPropertyInstant arbitraryConfig name monad
    = testProperty name $ forAll arbitraryConfig prop where
        prop :: c -> UTCTime -> Prices assets -> Portfolio assets -> Property
        prop config time prices portfolio
            = snd $ fromRight undefined
            $ runInitExecute config time prices portfolio monad

testInstrumentPropertyContinuous
    :: forall assets c s
     . (Labels assets, Show c, Instrument assets c s)
    => Gen c -> String
    -> Sem (RunExecuteEffects assets c s) Property
    -> TestTree
testInstrumentPropertyContinuous arbitraryConfig name monad
    = testProperty name
    $ forAll ((,) <$> arbitraryConfig <*> resize 5 arbitrary)
    $ uncurry prop where
        prop :: c -> TimeSeries (Prices assets) -> Portfolio assets -> Property
        prop config priceSeries initPortfolio
            = (totalValue initPrices initPortfolio > zero ==>)
            $ conjoin
            $ fromRight undefined $ run $ runError
            $ fmap fst $ runOutputList
            $ backtest' priceSeries initPortfolio config \exec -> do
                instantProp <- snd <$> runExecuteM monad
                Output.output instantProp
                exec
            where
                TimeSeries ((_, initPrices) :| _) = priceSeries

runInitExecute
    :: forall assets c s a
     . Instrument assets c s
    => c -> UTCTime -> Prices assets -> Portfolio assets
    -> Sem (RunExecuteEffects assets c s) a
    -> Either String (Portfolio assets, a)
runInitExecute config time prices
    = runExecute config state time prices where
        state = runInit prices config initState

runExecute
    :: forall assets c s a
     . Instrument assets c s
    => c -> s -> UTCTime -> Prices assets -> Portfolio assets
    -> Sem (RunExecuteEffects assets c s) a
    -> Either String (Portfolio assets, a)
runExecute config state time prices portfolio
    = run . runError
    . runInputConst prices
    . runMarketSimulation portfolio
    . runTimeConst time
    . fmap snd
    . runInstrument' config state

runExecuteM
    :: forall assets c s a r
     . (Instrument assets c s, Members (ExecuteEffects assets c s) r)
    => Sem (RunExecuteEffects assets c s) a
    -> Sem r (Portfolio assets, a)
runExecuteM monad = do
    IConfig config <- input
    IState state <- State.get
    time <- now
    prices <- input
    portfolio <- input
    fromEither $ runExecute config state time prices portfolio monad
