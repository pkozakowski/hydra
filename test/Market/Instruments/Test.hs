{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Market.Instruments.Test where

import Control.Monad
import Data.Either
import Data.Record.Hom
import Data.Time
import Market
import Market.Ops
import Market.Simulation
import Market.Types
import Market.Types.Test
import Numeric.Algebra hiding ((>))
import Polysemy
import Polysemy.Error
import Polysemy.Reader
import Polysemy.State as State
import Test.QuickCheck
import Test.QuickCheck.Instances.Time
import Test.QuickCheck.Property
import Test.Tasty
import Test.Tasty.QuickCheck

testInstrumentLaws
    :: forall assets c s. (Labels assets, Show c, Instrument assets c s)
    => Gen c -> TestTree
testInstrumentLaws arbitraryConfig = testGroup "Instrument"
    [ testProperty "Execute Idempotence"
        $ forAllConfigs $ executeIdempotence @assets
    , testProperty "Execute Efficiency"
        $ forAllConfigs $ executeEfficiency @assets
    ] where
        forAllConfigs :: Testable prop => (c -> prop) -> Property
        forAllConfigs = forAll arbitraryConfig

testInitAllocationExecuteAgreement
    :: forall assets c s. (Labels assets, Instrument assets c s, Show c)
    => Gen c -> TestTree
testInitAllocationExecuteAgreement arbitraryConfig
    = testProperty "initAllocation agrees with execute"
    $ forAll arbitraryConfig $ initAllocationExecuteAgreement @assets

executeIdempotence
    :: forall assets c s. (Labels assets, Instrument assets c s)
    => c -> UTCTime -> NominalDiffTime -> Prices assets -> Portfolio assets
    -> Property
executeIdempotence config time timeDiff prices portfolio
    =   totalValue prices portfolio > zero
    ==> portfolio' === portfolio'' where
        portfolio'
            = fromRight undefined $ runExecute config time prices portfolio
        portfolio''
            = fromRight undefined
            $ runExecute config time' prices portfolio' where
                time' = addUTCTime timeDiff time

data Sign = Plus | Zero | Minus deriving Eq

executeEfficiency
    :: forall assets c s. (Labels assets, Instrument assets c s)
    => c -> UTCTime -> Prices assets -> Portfolio assets -> Property
executeEfficiency config time prices portfolio
    =   totalValue prices portfolio > zero
    ==> isEfficient where
        isEfficient
            = fromRight undefined $ run $ runError
            $ runEfficiencyTest prices portfolio
            $ runInstrument config
            $ execute @_ @c @s
        runEfficiencyTest
            :: Prices assets
            -> Portfolio assets
            -> Sem
                 ( Market assets
                ': Reader (Portfolio assets)
                ': Reader (Prices assets)
                ': r
                 ) a
            -> Sem r Property
        runEfficiencyTest prices portfolio
            = runReader prices
            . runReader portfolio
            . fmap eitherToProperty
            . runError
            . runState (pure Zero :: HomRec assets Sign)
            . reinterpret2 \case
                Trade from to orderAmount -> do
                    signs <- State.get @(HomRec assets Sign)
                    let fromAmount = getIn from portfolio
                    when (absoluteAmount fromAmount orderAmount == zero)
                        $ throw
                        $ counterexample "trade with zero amount" False
                    assertSign from Plus signs
                    assertSign to Minus signs
                    put $ setIn from Minus $ setIn to Plus $ signs
                GetTime -> return time
            where
                assertSign
                    :: Member (Error Property) r
                    => LabelIn assets -> Sign -> HomRec assets Sign
                    -> Sem r ()
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

initAllocationExecuteAgreement
    :: forall assets c s. (Labels assets, Instrument assets c s)
    => c -> UTCTime -> Prices assets -> Portfolio assets -> Property
initAllocationExecuteAgreement config time prices portfolio
    =   totalValue prices portfolio > zero
    ==> valueAllocation prices portfolio'
    === (Just $ runInit prices config $ initAllocation) where
        portfolio'
            = fromRight undefined $ runExecute config time prices portfolio

runInit
    :: Instrument assets c s
    => Prices assets -> c -> Sem (InstrumentInitEffects assets c) a -> a
runInit prices config = run . runReader (IConfig config) . runReader prices

runExecute
    :: forall assets c s. Instrument assets c s
    => c -> UTCTime -> Prices assets -> Portfolio assets
    -> Either String (Portfolio assets)
runExecute config time prices portfolio
    = fmap fst $ run $ runError
    $ runReader prices
    $ runMarketSimulation time portfolio
    $ runInstrument config
    $ execute @_ @c @s
