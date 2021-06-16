module Test.Tasty.QuickCheck.Laws where

import Test.QuickCheck.Classes
import Test.Tasty
import Test.Tasty.QuickCheck

testLaws :: Laws -> TestTree
testLaws (Laws className props)
    = testGroup className $ uncurry testProperty <$> props
