module Test.QuickCheck.Pretty where

import Data.Text.Lazy (unpack)
import Text.Pretty.Simple
import Test.QuickCheck

counterexampleP
    :: (Show a, Testable prop)
    => String -> a -> prop -> Property
counterexampleP name value
    = counterexample
    $ name ++ " = " ++ unpack (pShow value)
