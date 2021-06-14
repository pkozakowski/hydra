{-# LANGUAGE TemplateHaskell #-}

import qualified Spec.Data.Record.Hom

import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "Specs"
    [ Spec.Data.Record.Hom.tests
    ]
