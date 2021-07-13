{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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
import Polysemy.State as State
import Test.QuickCheck
import Test.QuickCheck.Instances.Time
import Test.QuickCheck.Property
import Test.Tasty
import Test.Tasty.QuickCheck

testInstrumentLaws
    :: forall assets c i. (Labels assets, Instrument assets c i)
    => c -> TestTree
testInstrumentLaws config = testGroup "Instument"
    [ testProperty "InitAllocation <-> Execute Agreement"
        $ initAllocationExecuteAgreement @assets config
    , testProperty "Execute Idempotence" $ executeIdempotence @assets config
    , testProperty "Execute Efficiency" $ executeEfficiency @assets config
    ]

initAllocationExecuteAgreement
    :: forall assets c i. (Labels assets, Instrument assets c i)
    => c -> UTCTime -> Prices assets -> Portfolio assets -> Property
initAllocationExecuteAgreement config time prices portfolio
    =   totalValue prices portfolio > zero
    ==> valueAllocation prices portfolio'
    === (Just $ initAllocation prices config) where
        portfolio'
            = fromRight undefined $ runExecute config time prices portfolio

executeIdempotence
    :: forall assets c i. (Labels assets, Instrument assets c i)
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
    :: forall assets c i. (Labels assets, Instrument assets c i)
    => c -> UTCTime -> NominalDiffTime -> Prices assets -> Portfolio assets
    -> Property
executeEfficiency config time timeDiff prices portfolio
    =   totalValue prices portfolio > zero
    ==> isEfficient where
        isEfficient
            = fromRight undefined $ run $ runError
            $ runEfficiencyTest prices
            $ runInstrument prices config
            $ execute @_ @c @i prices portfolio
        runEfficiencyTest
            :: Prices assets -> Sem (Market assets ': r) a
            -> Sem r Property
        runEfficiencyTest prices
            = fmap eitherToProperty
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

runExecute
    :: forall assets c i. Instrument assets c i
    => c -> UTCTime -> Prices assets -> Portfolio assets
    -> Either String (Portfolio assets)
runExecute config time prices portfolio
    = fmap fst $ run $ runError
    $ runMarketSimulation time prices portfolio
    $ runInstrument prices config
    $ execute @_ @c @i prices portfolio
