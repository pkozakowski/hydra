{-# LANGUAGE TemplateHaskell #-}

import qualified Spec.Data.Record.Hom
import qualified Spec.Market.Instruments
import qualified Spec.Market.Ops
import qualified Spec.Market.Simulation
import qualified Spec.Market.Types

import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "Specs"
    -- Data:
    [ Spec.Data.Record.Hom.tests
    , Spec.Market.Types.tests
    -- Logic:
    , Spec.Market.Instruments.tests
    , Spec.Market.Ops.tests
    , Spec.Market.Simulation.tests
    ]
