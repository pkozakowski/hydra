import Spec.Data.Map.Default qualified
import Spec.Data.Map.Sparse qualified
import Spec.Data.Map.Static qualified
import Spec.Market.Feed.DB qualified
import Spec.Market.Feed.Dummy qualified
import Spec.Market.Instrument.Balance qualified
import Spec.Market.Instrument.Hold qualified
import Spec.Market.Instrument.Ops qualified
import Spec.Market.Ops qualified
import Spec.Market.Simulation qualified
import Spec.Market.Types qualified
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "Specs"
      -- Data:
      [ Spec.Data.Map.Default.tests
      , Spec.Data.Map.Sparse.tests
      , Spec.Data.Map.Static.tests
      , Spec.Market.Types.tests
      , -- Logic:
        Spec.Market.Feed.DB.tests
      , Spec.Market.Feed.Dummy.tests
      , Spec.Market.Instrument.Balance.tests
      , Spec.Market.Instrument.Hold.tests
      , Spec.Market.Instrument.Ops.tests
      , Spec.Market.Ops.tests
      , Spec.Market.Simulation.tests
      ]
