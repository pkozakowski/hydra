{-# LANGUAGE TemplateHaskell #-}

module Spec.Data.Map.Static where

import Data.Map.Static
import Data.Proxy
import Numeric.Algebra.Test
import Test.QuickCheck.Classes
import Test.Tasty
import Test.Tasty.QuickCheck.Laws
import Test.Tasty.TH

test_map_laws :: [TestTree]
test_map_laws = testLaws <$>
    [ readMapLaws @Int @Int @(StaticMap Int Int)
    , setMapLaws @Int @Int @(StaticMap Int Int)
    , buildMapLaws @Int @Int @(StaticMap Int Int)
    ]

test_standard_laws :: [TestTree]
test_standard_laws = testLaws <$>
    [ functorLaws pStaticMapF
    , foldableLaws pStaticMapF
    , traversableLaws pStaticMapF
    , showLaws pStaticMap
    ] where
        pStaticMap = Proxy :: Proxy (StaticMap Int Int)
        pStaticMapF = Proxy :: Proxy (StaticMap Int)

-- Apply, Eq and numeric laws are not tested, because those instances depend on
-- <.>, which requires both maps to share the set of keys, causing runtime
-- errors for arbitrary maps.

tests :: TestTree
tests = $(testGroupGenerator)
