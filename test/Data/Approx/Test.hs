module Data.Approx.Test where

import Data.Approx
import Test.QuickCheck

runOp :: (Approx a, Show a) => Bool -> a -> a -> Property
runOp isEq x y = counterexample (show x ++ opStr ++ show y) success where
    opStr = case isEq == success of
        True  -> " =~ "
        False -> " /~ "
    success = x `op` y where
        op = if isEq then (=~) else (/~)

(==~) :: (Approx a, Show a) => a -> a -> Property
(==~) = runOp True
infix 4 ==~

(=/~) :: (Approx a, Show a) => a -> a -> Property
(=/~) = runOp False
infix 4 =/~
