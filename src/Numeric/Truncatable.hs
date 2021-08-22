{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Numeric.Truncatable where

import Data.Fixed
import qualified Data.Ratio as Ratio
import Numeric.Field.Fraction

class Truncatable a where
    truncateTo :: forall r. HasResolution r => r -> a -> a

instance Truncatable (Fraction Integer) where

    truncateTo
        :: forall r
         . HasResolution r
        => r
        -> Fraction Integer
        -> Fraction Integer
    truncateTo res frac = fixedToFraction $ fixed where
        fixed :: Fixed r
        fixed = fromRational $ numerator frac Ratio.% denominator frac

fixedToFraction :: HasResolution r => Fixed r -> Fraction Integer
fixedToFraction fixed@(MkFixed num) = num % resolution fixed
