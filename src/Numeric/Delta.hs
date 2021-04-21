{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Numeric.Delta where

import Data.Coerce
import Language.Haskell.TH
import Numeric.Absolute
import Numeric.Algebra
import Numeric.Relative
import Prelude hiding ((+), (-))

-- | Pairing between an Absolute and a Relative type, which are Modules over the
-- same scalar type. A Relative value represents the difference between two
-- Absolute values. In some cases, it can be integrated back into an Absolute value.
--
class (Absolute a b, Relative a c) => Delta a b c | b -> c, c -> b where

    -- | Difference between two Absolutes gives a Relative.
    --
    delta :: b -> b -> c

    -- | Opportunity to reintegrate a Relative into an Absolute. May fail.
    --
    sigma :: b -> c -> Maybe b

-- | Derive an instance for a comparable Relative with the same representation as an Absolute.
--
deriveDeltaOrd :: Name -> Name -> Name -> Q [Dec]
deriveDeltaOrd scr abs rel =
    [d| instance Delta $(conT scr) $(conT abs) $(conT rel) where
            delta x y = coerce x - coerce y
            sigma x y = if z >= zero then Just (coerce z) else Nothing
                where z = coerce x + y |]
