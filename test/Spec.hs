{-# LANGUAGE TemplateHaskell #-}

import qualified Spec.Data.Record.Hom
import qualified Spec.Market.Ops
import qualified Spec.Market.Test
import qualified Spec.Market.Types

import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "Specs"
    -- Data:
    [ Spec.Data.Record.Hom.tests
    , Spec.Market.Types.tests
    -- Test infra:
    , Spec.Market.Test.tests
    -- Logic:
    , Spec.Market.Ops.tests
    ]
