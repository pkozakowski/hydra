{-# LANGUAGE TemplateHaskell #-}

import qualified Spec.Data.Map.Default
import qualified Spec.Data.Map.Sparse
import qualified Spec.Data.Map.Static
import qualified Spec.Market.Evaluation
import qualified Spec.Market.Instruments
import qualified Spec.Market.Ops
import qualified Spec.Market.Simulation
import qualified Spec.Market.Types

import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "Specs"
    -- Data:
    [ Spec.Data.Map.Default.tests
    , Spec.Data.Map.Sparse.tests
    , Spec.Data.Map.Static.tests
    , Spec.Market.Types.tests
    -- Logic:
    , Spec.Market.Evaluation.tests
    , Spec.Market.Instruments.tests
    , Spec.Market.Ops.tests
    , Spec.Market.Simulation.tests
    ]
