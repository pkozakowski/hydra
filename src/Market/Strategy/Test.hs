module Market.Strategy.Test where

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

strategyLaws
  :: forall c s
   . (Show c, Strategy c s)
  => Gen c
  -> Gen c
  -> Laws
strategyLaws arbitraryApproxConfig arbitraryExactConfig =
  Laws
    "Strategy"
    [
      ( "Idempotence (Instant)"
      , instApprox arbitrary $ idempotence @c @s
      )
    ,
      ( "Idempotence (Continuous)"
      , contApprox arbitrary $ idempotence @c @s
      )
    ,
      ( "Efficiency (Instant)"
      , instApprox arbitrary $ efficiency @c @s
      )
    ,
      ( "Efficiency (Continuous)"
      , contApprox arbitrary $ efficiency @c @s
      )
    ,
      ( "Allocation Agreement (Instant)"
      , instExact (pure zeroFees) $ allocationAgreement @c @s
      )
    ,
      ( "Allocation Agreement (Continuous)"
      , contExact (pure zeroFees) $ allocationAgreement @c @s
      )
    ]
  where
    instApprox = strategyPropertyInstant arbitraryApproxConfig
    contApprox = strategyPropertyContinuous arbitraryApproxConfig
    instExact = strategyPropertyInstant arbitraryExactConfig
    contExact = strategyPropertyContinuous arbitraryExactConfig

idempotence
  :: forall c s r
   . ( Strategy c s
     , Members (ExecuteEffects c s) r
     )
  => Sem r Property
idempotence = whenNotBroke @c @s do
  (portfolio', portfolio'') <-
    snd <$> runExecuteM @c do
      execute @c @s
      portfolio' <- input @Portfolio
      execute @c @s
      portfolio'' <- input @Portfolio
      return (portfolio', portfolio'')
  return $ portfolio' === portfolio''

data Sign = Plus | Zero | Minus deriving (Eq)

instance Default Sign where
  def = Zero

type RunExecuteEffects c s =
  [ State (SState s)
  , Input (SConfig c)
  , Time
  , Market
  , Input Portfolio
  , Input Prices
  , Input Fees
  , Error MarketError
  ]

efficiency
  :: forall c s r
   . ( Strategy c s
     , Members (ExecuteEffects c s) r
     )
  => Sem r Property
efficiency = whenNotBroke @c @s $ runEfficiencyTestM (execute @c @s)
  where
    runEfficiencyTestM
      :: Sem (RunExecuteEffects c s) ()
      -> Sem r Property
    runEfficiencyTestM monad = do
      SConfig config <- input
      SState state <- State.get
      time <- now
      portfolio <- input @Portfolio
      fees <- input @Fees
      subsume_
        $ fmap eitherToProperty
        $ runError
        $ runState (const Zero `remap` portfolio :: SparseMap Asset Sign)
        $ reinterpret2 \case
          Trade from to orderAmount -> do
            signs <- State.get @(SparseMap Asset Sign)
            let absAmount =
                  absoluteAmount
                    fees
                    from
                    (portfolio ! from)
                    orderAmount
            assertSign from Plus signs
            assertSign to Minus signs
            put $ set from Minus $ set to Plus $ signs
        $ runTimeConst time
        $ runStrategy' config state
        $ monad
      where
        assertSign
          :: Member (Error Property) r'
          => Asset
          -> Sign
          -> SparseMap Asset Sign
          -> Sem r' ()
        assertSign asset sign signs =
          when (signs ! asset == sign) $
            throw $
              counterexample
                ("incoherent trades for " ++ show asset)
                False
        eitherToProperty = \case
          Right _ -> property succeeded
          Left prop -> prop
        isZero = \case
          Absolute x -> x == zero
          Relative (Share x) -> x == zero

allocationAgreement
  :: forall c s r
   . ( Strategy c s
     , Members (ExecuteEffects c s) r
     )
  => Sem r Property
allocationAgreement = whenNotBroke @c @s do
  SConfig config <- input @(SConfig c)
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
  if totalValue prices portfolio > zero
    then monad
    else return discard

runInit
  :: Strategy c s
  => Prices
  -> c
  -> Sem (InitEffects c) a
  -> a
runInit prices config =
  run . runInputConst (SConfig config) . runInputConst prices

strategyPropertyInstant
  :: forall c s
   . (Show c, Strategy c s)
  => Gen c
  -> Gen Fees
  -> Sem (RunExecuteEffects c s) Property
  -> Property
strategyPropertyInstant arbitraryConfig arbitraryFees monad =
  forAll ((,) <$> arbitraryConfig <*> arbitraryFees) $
    uncurry prop
  where
    prop :: c -> Fees -> UTCTime -> Prices -> Portfolio -> Property
    prop config fees time prices portfolio =
      snd $
        handleMarketErrors $
          runInitExecute config time fees prices portfolio monad

strategyPropertyContinuous
  :: forall c s
   . (Show c, Strategy c s)
  => Gen c
  -> Gen Fees
  -> Sem (RunExecuteEffects c s) Property
  -> Property
strategyPropertyContinuous arbitraryConfig arbitraryFees monad =
  forAll ((,,) <$> arbitraryConfig <*> arbitraryFees <*> resize 5 arbitrary) $
    uncurry3 prop
  where
    prop :: c -> Fees -> TimeSeries Prices -> Portfolio -> Property
    prop config fees priceSeries initPortfolio =
      (totalValue initPrices initPortfolio > zero ==>) $
        conjoin $
          handleMarketErrors $
            run $
              runError @MarketError $
                fmap fst $
                  runOutputList @Property $
                    runPrecisionExact $
                      backtest' @c @s fees priceSeries initPortfolio config \exec -> do
                        instantProp <- snd <$> runExecuteM monad
                        Output.output instantProp
                        exec
      where
        TimeSeries ((_, initPrices) :| _) = priceSeries
    uncurry3 f (x, y, z) = f x y z

handleMarketErrors :: Either MarketError a -> a
handleMarketErrors = either handle id
  where
    handle = \case
      InsufficientBalanceToCoverFees _ _ -> discard
      e -> error $ show e

runInitExecute
  :: forall c s a
   . Strategy c s
  => c
  -> UTCTime
  -> Fees
  -> Prices
  -> Portfolio
  -> Sem (RunExecuteEffects c s) a
  -> Either MarketError (Portfolio, a)
runInitExecute config time fees prices =
  runExecute config state time fees prices
  where
    state = runInit prices config initState

runExecute
  :: forall c s a
   . Strategy c s
  => c
  -> s
  -> UTCTime
  -> Fees
  -> Prices
  -> Portfolio
  -> Sem (RunExecuteEffects c s) a
  -> Either MarketError (Portfolio, a)
runExecute config state time fees prices portfolio =
  run
    . runError
    . runInputConst fees
    . runInputConst prices
    . runMarketSimulation portfolio
    . runTimeConst time
    . fmap snd
    . runStrategy' config state

runExecuteM
  :: forall c s a r
   . (Strategy c s, Members (ExecuteEffects c s) r)
  => Sem (RunExecuteEffects c s) a
  -> Sem r (Portfolio, a)
runExecuteM monad = do
  SConfig config <- input
  SState state <- State.get
  time <- now
  fees <- input
  prices <- input
  portfolio <- input
  fromEither $ runExecute config state time fees prices portfolio monad
