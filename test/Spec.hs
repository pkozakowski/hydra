{-# LANGUAGE TemplateHaskell #-}

import qualified Spec.Data.Record.Hom
import qualified Spec.Market.Types

import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "Specs"
    [ Spec.Data.Record.Hom.tests
    , Spec.Market.Types.tests
    ]
