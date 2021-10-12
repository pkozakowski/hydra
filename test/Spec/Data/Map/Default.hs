{-# LANGUAGE TemplateHaskell #-}

module Spec.Data.Map.Default where

import Data.List (nub, sort)
import Data.Proxy
import Data.Map.Default
import Numeric.Algebra.Test
import Test.QuickCheck.Classes
import Test.Tasty
import Test.Tasty.QuickCheck hiding (labels)
import Test.Tasty.QuickCheck.Laws
import Test.Tasty.TH

test_map_laws :: [TestTree]
test_map_laws = testLaws <$>
    [ readMapLaws @Int @Int @(DefaultMap Int Int)
    , setMapLaws @Int @Int @(DefaultMap Int Int)
    , readWriteMapLaws @Int @Int @(DefaultMap Int Int)
    ]

test_standard_laws :: [TestTree]
test_standard_laws = testLaws <$>
    [ functorLaws pDefaultMapF
    , applyLaws pDefaultMapF
    , foldableLaws pDefaultMapF
    , eqLaws pDefaultMap
    , showLaws pDefaultMap
    ] where
        pDefaultMap = Proxy :: Proxy (DefaultMap Int Int)
        pDefaultMapF = Proxy :: Proxy (DefaultMap Int)

test_numeric_laws :: [TestTree]
test_numeric_laws = testLaws <$>
    [ additiveLaws pDefaultMap
    , abelianLaws pDefaultMap
    , leftModuleLaws pInteger pDefaultMap
    , rightModuleLaws pInteger pDefaultMap
    ] where
        pDefaultMap = Proxy :: Proxy (DefaultMap Int Int)
        pInteger = Proxy :: Proxy Integer

tests :: TestTree
tests = $(testGroupGenerator)
