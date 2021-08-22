{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.Data.Record.Hom where

import Data.List
import Data.Proxy
import Data.Record.Hom
import Data.Record.Hom.Test
import Numeric.Algebra.Test
import Test.QuickCheck.Classes
import Test.Tasty
import Test.Tasty.QuickCheck hiding (labels)
import Test.Tasty.QuickCheck.Laws
import Test.Tasty.TH

type ThreeLabels = '["a", "b", "c"]

prop_get_inverses_set_a :: Int -> HomRec ThreeLabels Int -> Bool
prop_get_inverses_set_a x r = get #a (set #a x r) == x

prop_get_inverses_set_b :: Int -> HomRec ThreeLabels Int -> Bool
prop_get_inverses_set_b x r = get #b (set #b x r) == x

prop_set_inverses_get_a :: HomRec ThreeLabels Int -> Bool
prop_set_inverses_get_a r = set #a (get #a r) r == r

prop_set_inverses_get_b :: HomRec ThreeLabels Int -> Bool
prop_set_inverses_get_b r = set #b (get #b r) r == r

prop_getIn_inverses_setIn :: LabelIn ThreeLabels -> Int -> HomRec ThreeLabels Int -> Bool
prop_getIn_inverses_setIn li x r = getIn li (setIn li x r) == x

prop_setIn_inverses_getIn :: LabelIn ThreeLabels -> HomRec ThreeLabels Int -> Bool
prop_setIn_inverses_getIn li r = setIn li (getIn li r) r == r

prop_toList_preserves_empty :: Bool
prop_toList_preserves_empty = toList Empty == ([] :: [(LabelIn '[], Int)])

prop_toList_preserves_cons :: Int -> Bool
prop_toList_preserves_cons x = toList (#a := x :& Empty) == zip labels [x]

prop_fromList_inverses_toList :: HomRec ThreeLabels Int -> Bool
prop_fromList_inverses_toList r = fromList undefined (toList r) == r

prop_labels_are_unique :: Bool
prop_labels_are_unique = nub lbs == lbs where
    lbs = labels :: [LabelIn ThreeLabels]

prop_labels_have_correct_length :: Bool
prop_labels_have_correct_length = length lbs == 3 where
    lbs = labels :: [LabelIn ThreeLabels]

test_standard_laws :: [TestTree]
test_standard_laws = testLaws <$>
    [ functorLaws pHomRecF
    , applicativeLaws pHomRecF
    , foldableLaws pHomRecF
    , traversableLaws pHomRecF
    , eqLaws pHomRec
    , showLaws pHomRec
    ] where
        pHomRec = Proxy :: Proxy (HomRec ThreeLabels Integer)
        pHomRecF = Proxy :: Proxy (HomRec ThreeLabels)

test_numeric_laws :: [TestTree]
test_numeric_laws = testAllModuleLaws pInteger pHomRec where
    pHomRec = Proxy :: Proxy (HomRec ThreeLabels Integer)
    pInteger = Proxy :: Proxy Integer

tests :: TestTree
tests = $(testGroupGenerator)
