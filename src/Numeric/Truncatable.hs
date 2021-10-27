{-# LANGUAGE InstanceSigs #-}

module Numeric.Truncatable where

import Data.Fixed
import Data.Proxy
import qualified Data.Ratio as Ratio
import Numeric.Algebra ((+), (-))
import Numeric.Field.Fraction
import Prelude hiding ((+), (-))

class Truncatable a where
    truncateTo :: forall r. HasResolution r => r -> a -> a

instance Truncatable (Fraction Integer) where

    truncateTo
        :: forall r
         . HasResolution r
        => r
        -> Fraction Integer
        -> Fraction Integer
    truncateTo res frac
        = if frac - lower < upper - frac then lower else upper where
            lower = fixedToFraction $ fix $ numerator frac
            upper = fixedToFraction $ fix $ numerator frac + 1

            fix :: Integer -> Fixed r
            fix num = fromRational $ num Ratio.% denominator frac

fixedToFraction :: HasResolution r => Fixed r -> Fraction Integer
fixedToFraction fixed@(MkFixed num) = num % resolution fixed

realToFraction :: Real a => a -> Fraction Integer
realToFraction = ratioToFraction . toRational

ratioToFraction :: Ratio.Ratio Integer -> Fraction Integer
ratioToFraction = (%) <$> Ratio.numerator <*> Ratio.denominator

instance HasResolution r => HasResolution (Proxy r) where
    resolution :: p (Proxy r) -> Integer
    resolution _ = resolution $ Proxy @r
