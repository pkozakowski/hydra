{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Numeric.Kappa where

import Language.Haskell.TH
import Numeric.Algebra
import Numeric.Algebra.Division
import Prelude hiding ((*), (/))

-- | Triple of numeric types, where one is a quotient of two others. There are two such quotients,
-- and they can be multiplied to obtain the third type.
--
class Kappa a b c | a b -> c, a c -> b, b c -> a where

    -- | Quotient of a and b gives a c.
    --
    kappa :: a -> b -> c

    -- | The other quotient.
    --
    kappa' :: a -> c -> b

    -- | Product of b and c gives an a.
    --
    pi :: b -> c -> a

-- | Derive an instance for three types coercible to a divisible type.
--
deriveKappaDivision :: Name -> Name -> Name -> Name -> Q [Dec]
deriveKappaDivision scr a b c =
    [d| instance Kappa $(conT a) $(conT b) $(conT c) where
            kappa x y = coerce (coerce x / coerce y :: $(conT scr))
            kappa' x z = coerce (coerce x / coerce z :: $(conT scr))
            pi y z = coerce (coerce y * coerce z :: $(conT scr)) |]
