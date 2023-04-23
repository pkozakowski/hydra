{-# LANGUAGE TemplateHaskell #-}

import qualified Spec.Data.Map.Default
import qualified Spec.Data.Map.Sparse
import qualified Spec.Data.Map.Static
import qualified Spec.Market.Feed.Dummy
import qualified Spec.Market.Instrument.Balance
import qualified Spec.Market.Instrument.Hold
import qualified Spec.Market.Instrument.Ops
import qualified Spec.Market.Ops
import qualified Spec.Market.Simulation
import qualified Spec.Market.Types
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "Specs"
      -- Data:
      [ Spec.Data.Map.Default.tests,
        Spec.Data.Map.Sparse.tests,
        Spec.Data.Map.Static.tests,
        Spec.Market.Types.tests,
        -- Logic:
        Spec.Market.Feed.Dummy.tests,
        Spec.Market.Instrument.Balance.tests,
        Spec.Market.Instrument.Hold.tests,
        Spec.Market.Instrument.Ops.tests,
        Spec.Market.Ops.tests,
        Spec.Market.Simulation.tests
      ]
