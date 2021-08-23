{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}

module Numeric.Truncatable where

import Data.Fixed
import Data.Proxy
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

realToFraction :: Real a => a -> Fraction Integer
realToFraction = ratioToFraction . toRational

ratioToFraction :: Ratio.Ratio Integer -> Fraction Integer
ratioToFraction = (%) <$> Ratio.numerator <*> Ratio.denominator

instance HasResolution r => HasResolution (Proxy r) where
    resolution :: p (Proxy r) -> Integer
    resolution _ = resolution $ Proxy @r
