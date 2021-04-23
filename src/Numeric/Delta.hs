{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Numeric.Delta where

import Data.Coerce
import Language.Haskell.TH
import Numeric.Algebra
import Numeric.Relative
import Prelude hiding ((+), (-))

-- | Pairing between two types, where the second is a Relative and represents
-- the difference of two values of the first type. In some cases, it can be
-- integrated back into a value of the first type.
--
class Relative a c => Delta a b c | b -> c, c -> b where

    -- | Difference between two b gives a c.
    --
    delta :: b -> b -> c

    -- | Opportunity to reintegrate a c into a b. May fail.
    --
    sigma :: b -> c -> Maybe b

-- | Derive an instance for a comparable Relative with the same representation as the other type.
--
deriveDeltaOrd :: Name -> Name -> Name -> Q [Dec]
deriveDeltaOrd scr abs rel =
    [d| instance Delta $(conT scr) $(conT abs) $(conT rel) where
            delta x y = coerce x - coerce y
            sigma x y = if z >= zero then Just (coerce z) else Nothing
                where z = coerce x + y |]
