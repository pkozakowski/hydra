{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE InstanceSigs #-}

module Market.Strategy.Balance where

import Control.Monad
import Data.Bifunctor
import Data.Functor.Apply
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Static hiding (Value, null)
import Data.Maybe
import qualified Data.Sequence as Seq
import Data.Time
import qualified Dhall as Dh
import qualified Dhall.Core as Dh
import qualified Dhall.Map as Dh
import GHC.Generics
import GHC.Stack
import Market
import Market.Dhall
import Market.Ops
import Market.Simulation
import Market.Strategy.Ops
import Market.Strategy.Some
import Market.Time
import Numeric.Algebra hiding (fromInteger, (<), (>))
import Numeric.Kappa
import Numeric.Normed
import Numeric.Truncatable
import Polysemy
import Polysemy.Input
import Polysemy.State
import Prelude hiding ((+))

data BalanceConfig = BalanceConfig
  { configs :: StaticMap StrategyName SomeStrategyConfig
  , target :: Distribution StrategyName
  , tolerance :: Scalar
  , updateEvery :: NominalDiffTime
  }
  deriving (Generic, Show)

data BalanceState = BalanceState
  { states :: StaticMap StrategyName SomeStrategiestate
  , allocations :: StaticMap StrategyName (Distribution Asset)
  , lastUpdateTime :: UTCTime
  }

instance Truncatable BalanceState where
  truncateTo res =
    BalanceState
      <$> truncateTo res . states
      <*> truncateTo res . allocations
      <*> lastUpdateTime

instance Strategy BalanceConfig BalanceState where
  initState = do
    SConfig config <- input @(SConfig (BalanceConfig))
    prices <- input @Prices
    states <-
      multiplexConfig (configs config) $
        initState @SomeStrategyConfig
    allocations <-
      multiplexConfig (configs config) $
        initAllocation @SomeStrategyConfig
    return
      BalanceState
        { states = states
        , allocations = allocations
        , lastUpdateTime = UTCTime (ModifiedJulianDay 0) 0
        }

  initAllocation = do
    SConfig config <- input @(SConfig (BalanceConfig))
    allocations <-
      multiplexConfig (configs config) $
        initAllocation @SomeStrategyConfig
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
    SConfig config <- input @(SConfig (BalanceConfig))
    SState state <- get
    when (shouldUpdate prices portfolio time config state) do
      -- 1. Compute the ideal per-Strategy portfolios according to the
      -- value allocations.
      let portfolios = distributePortfolio config state prices portfolio
      -- 2. Execute the per-Strategy trades in simulated markets to get
      -- new portfolios.
      fees <- input @Fees @r
      let exec = execute @SomeStrategyConfig @SomeStrategiestate
          executions =
            fmap
              ( fmap fst
                  -- Forward the real fees to the Strategies.
                  . inject fees
              )
              $ (\c s -> runStrategy' c s exec)
                <$> configs config
                  <.> states state
      portfoliosAndStrategies' <-
        -- Run the simulation with zero fees, so the Strategies don't
        -- have to allocate funds for them.
        inject zeroFees $
          sequence $
            runMarketSimulation <$> portfolios <.> executions
      let portfolios' = fst <$> portfoliosAndStrategies'
          states' = snd <$> portfoliosAndStrategies'
          allocations' =
            valueAllocOr prices <$> portfolios' <.> allocations state
      -- 3. Make balancing trades between the old and new global
      -- portfolios.
      let portfolio' = foldl (+) zero portfolios'
          allocation' = valueAlloc prices portfolio'
      allocationToTrades (tolerance config) allocation'
      -- 4. Update the state.
      put $
        SState $
          BalanceState
            { states = states'
            , allocations = allocations'
            , lastUpdateTime = time
            }
    where
      shouldUpdate prices portfolio time config state =
        totalValue prices portfolio > zero
          && diff > updateEvery config
        where
          diff = time `diffUTCTime` lastUpdateTime state

      valueAlloc
        :: HasCallStack
        => Prices
        -> Portfolio
        -> Distribution Asset
      valueAlloc prices = fromJust . valueAllocation prices

      valueAllocOr prices portfolio allocation =
        fromMaybe allocation $ valueAllocation prices portfolio

      inject
        :: forall i r a
         . Member (Input i) r
        => i
        -> Sem r a
        -> Sem r a
      inject inp = intercept @(Input i) \case
        Input -> pure inp

  visit
    :: forall self agg
     . Prices
    -> Portfolio
    -> BalanceConfig
    -> BalanceState
    -> Visitor self agg
  visit prices portfolio config state visitAgg visitSelf =
    visitAgg (visitSelf prices portfolio config state) $
      visit' visitAgg visitSelf prices
        <$> distributePortfolio config state prices portfolio
          <.> configs config
          <.> states state

  managedAssets config =
    nub $ managedAssets =<< snd <$> toList (configs config)

  smartEmbed config =
    Dh.MultiLet
      ( import_ ["Market", "Strategy", "Balance"] "balance"
          :| import_ ["Market", "Strategy", "Map"] "Strategy"
          : import_ ["Market", "Strategy", "Map"] "share"
          : import_ ["Duration"] "days"
          : import_ ["Duration"] "hours"
          : import_ ["Duration"] "minutes"
          : import_ ["Duration"] "seconds"
          : subconfigImports
      )
      $ call "balance"
      $ Dh.RecordLit
      $ Dh.fromList
      $ second Dh.makeRecordField
        <$> [ ("configs", configsDh)
            , ("target", targetDh)
            , ("tolerance", toleranceDh)
            , ("updateEvery", updateEveryDh)
            ]
    where
      smartSubconfigs = smartEmbed <$> configs config

      subconfigImports =
        concat $
          fmap (NonEmpty.toList . bindings . snd) $
            toList smartSubconfigs
        where
          bindings (Dh.MultiLet b _) = b

      configsDh =
        Dh.ListLit Nothing $
          Seq.fromList $
            fmap strategy $
              toList smartSubconfigs
        where
          strategy (StrategyName name, Dh.MultiLet _ expr) =
            call2 "Strategy" (embedString name) expr

      targetDh =
        Dh.ListLit Nothing $
          Seq.fromList $
            fmap share $
              toList dist
        where
          Distribution dist = target config
          share (StrategyName name, Share shr) =
            call2 "share" (embedString name) $
              embedScalar shr

      toleranceDh = embedScalar $ tolerance config

      updateEveryDh =
        if truncError day < 1
          then callN "days" $ period `div` day
          else
            if truncError hour < 1
              then callN "hours" $ period `div` hour
              else
                if truncError minute < 1
                  then callN "minutes" $ period `div` minute
                  else callN "seconds" $ period `div` second
        where
          (-) = (Prelude.-)
          (*) = (Prelude.*)
          (/) = (Prelude./)
          truncError upTo =
            period - (period `div` upTo `mul` upTo)
          callN name =
            call name . Dh.NaturalLit . fromInteger
          div period upTo = floor $ period / upTo
          mul n period = fromInteger n * period
          day = 24 * hour
          hour = 60 * minute
          minute = 60 * second
          second = secondsToNominalDiffTime 1
          period = updateEvery config

distributePortfolio
  :: BalanceConfig
  -> BalanceState
  -> Prices
  -> Portfolio
  -> StaticMap StrategyName Portfolio
distributePortfolio config state prices portfolio =
  idealPortfolio prices <$> targetValues `reapplyOuter` allocations state
  where
    idealPortfolio prices value allocation =
      fromJust $ value `scale` allocation `kappa` prices
    Distribution targetShares = target config
    Values targetValues =
      totalValue prices portfolio `scale` target config

multiplexConfig
  :: Traversable f
  => f c
  -> Sem (Input (SConfig c) : r) a
  -> Sem r (f a)
multiplexConfig configs monad =
  sequence $ flip runInputConst monad <$> SConfig <$> configs
