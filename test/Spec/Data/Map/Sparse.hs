{-# LANGUAGE TemplateHaskell #-}

module Spec.Data.Map.Sparse where

import Data.List (nub, sort)
import Data.Proxy
import Data.Map.Sparse
import Numeric.Algebra.Test
import Test.QuickCheck.Classes
import Test.Tasty
import Test.Tasty.QuickCheck hiding (labels)
import Test.Tasty.QuickCheck.Laws
import Test.Tasty.TH

test_map_laws :: [TestTree]
test_map_laws = testLaws <$>
    [ readMapLaws @Int @Int @(SparseMap Int Int)
    , setMapLaws @Int @Int @(SparseMap Int Int)
    , readWriteMapLaws @Int @Int @(SparseMap Int Int)
    , buildMapLaws @Int @Int @(SparseMap Int Int)
    ]

test_standard_laws :: [TestTree]
test_standard_laws = testLaws <$>
    [ eqLaws pSparseMap
    , showLaws pSparseMap
    ] where
        pSparseMap = Proxy :: Proxy (SparseMap Int Int)
        pSparseMapF = Proxy :: Proxy (SparseMap Int)

-- Functor, Apply, Foldable and Traversable are not tested, because the Laws
-- quantify over values, and the instance Arbitrary (SparseMap k v) requires
-- Default v. But they're satisfied by virtue of DefaultMap tests.

test_numeric_laws :: [TestTree]
test_numeric_laws = testLaws <$> allModuleLaws pInteger pSparseMap where
    pSparseMap = Proxy :: Proxy (SparseMap Int Int)
    pInteger = Proxy :: Proxy Integer

tests :: TestTree
tests = $(testGroupGenerator)
