{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Numeric.Kappa where

import Language.Haskell.TH
import Numeric.Algebra
import Numeric.Algebra.Division
import Prelude hiding ((*), (/))

-- | Triple of numeric types, where one is a quotient of two others. There are two such quotients,
-- and they can be multiplied to obtain the third type. Division by zero gives Nothing.
class Kappa a b c | a b -> c, a c -> b, b c -> a where

    -- | Quotient of a and b gives c.
    kappa :: a -> b -> Maybe c

    -- | The other quotient.
    kappa' :: a -> c -> Maybe b

    -- | Product of b and c gives a.
    pi :: b -> c -> a

-- | Derive an instance for three types coercible to a divisible type.
deriveKappaDivision :: Name -> Name -> Name -> Name -> Q [Dec]
deriveKappaDivision scr a b c =
    [d| instance Kappa $(conT a) $(conT b) $(conT c) where
            kappa x y = coerce (coerce x `safeDiv` coerce y :: Maybe $(conT scr))
            kappa' x z = coerce (coerce x `safeDiv` coerce z :: Maybe $(conT scr))
            pi y z = coerce (coerce y * coerce z :: $(conT scr)) |]

safeDiv :: (Division a, Monoidal a, Eq a) => a -> a -> Maybe a
safeDiv x y = if y == zero then Nothing else Just $ x / y
