{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module Market
  ( module Market
  , module Market.Types
  ) where

import Control.DeepSeq
import Data.Fixed
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Static
import Data.Proxy
import Data.Void
import Dhall.Core qualified as Dh
import Dhall.Src qualified as Dh
import GHC.Generics
import Market.Time
import Market.Types
import Numeric.Truncatable
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Internal
import Polysemy.State as State

data Market m a where
  Trade :: Asset -> Asset -> OrderAmount -> Market m ()

makeSem ''Market

newtype SConfig c = SConfig {unSConfig :: c}

newtype SState s = SState {unSState :: s}
  deriving newtype (Truncatable)

data MarketError
  = InsufficientBalanceForTransfer SomeAmount Portfolio
  | InsufficientBalanceToCoverFees Fees Portfolio
  | OtherError String
  deriving (Generic, NFData)

instance Show MarketError where
  show = \case
    InsufficientBalanceForTransfer someAmount portfolio ->
      "insufficient balance for transfer: "
        ++ show someAmount
        ++ "; portfolio: "
        ++ show portfolio
    InsufficientBalanceToCoverFees fees portfolio ->
      "insufficient balance to cover fees: "
        ++ show fees
        ++ "; portfolio: "
        ++ show portfolio
    OtherError s -> s

type InitEffects c =
  [ Input Prices
  , Input (SConfig c)
  ]

type ExecuteEffects c s =
  [ Market
  , Time
  , Input Fees
  , Input Prices
  , Input Portfolio
  , Input (SConfig c)
  , State (SState s)
  , Error MarketError
  ]

type AggregateVisitor self agg =
  self -> StaticMap StrategyName agg -> agg

type SelfVisitor (self :: *) =
  forall c s
   . Strategy c s
  => Prices
  -> Portfolio
  -> c
  -> s
  -> self

type Visitor (self :: *) (agg :: *) =
  AggregateVisitor self agg
  -> SelfVisitor self
  -> agg

class Truncatable s => Strategy c s | s -> c, c -> s where
  initState :: Members (InitEffects c) r => Sem r s
  initAllocation :: Members (InitEffects c) r => Sem r (Distribution Asset)
  execute :: Members (ExecuteEffects c s) r => Sem r ()
  visit :: Prices -> Portfolio -> c -> s -> Visitor self agg
  managedAssets :: c -> [Asset]
  smartEmbed :: c -> Dh.MultiLet Dh.Src Dh.Import

visit'
  :: forall self agg c s
   . Strategy c s
  => AggregateVisitor self agg
  -> SelfVisitor self
  -> Prices
  -> Portfolio
  -> c
  -> s
  -> agg
visit' visitAgg visitSelf prices portfolio config state =
  visit prices portfolio config state visitAgg visitSelf

runStrategy
  :: forall c s r a
   . (Strategy c s, Member (Input Prices) r)
  => c
  -> Sem (State (SState s) ': Input (SConfig c) ': r) a
  -> Sem r (s, a)
runStrategy config monad = do
  state <- runInputConst (SConfig config) initState
  runStrategy' config state monad

runStrategy'
  :: forall c s r a
   . (Strategy c s, Member (Input Prices) r)
  => c
  -> s
  -> Sem (State (SState s) ': Input (SConfig c) ': r) a
  -> Sem r (s, a)
runStrategy' config state monad = runInputConst (SConfig config) do
  (SState state', x) <- runState (SState state) monad
  return (state', x)

smartToDhall :: Strategy c s => c -> Dh.Expr Dh.Src Dh.Import
smartToDhall config = Dh.wrapInLets bindings' expr
  where
    bindings' = NonEmpty.sort $ NonEmpty.nub bindings
    Dh.MultiLet bindings expr = smartEmbed config
