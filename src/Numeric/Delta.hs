{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Numeric.Delta where

import Data.Coerce
import Language.Haskell.TH
import Numeric.Algebra
import Prelude hiding ((+), (-))

-- | Pairing between two types, where the second is a Group and represents
-- the difference of two values of the first type. In some cases, it can be
-- integrated back into a value of the first type.
class Group b => Delta a b | a -> b, b -> a where

    -- | Difference between two a gives b.
    delta :: a -> a -> b

    -- | Opportunity to reintegrate b into a. May fail.
    sigma :: a -> b -> Maybe a

-- | Derive an instance for a comparable Relative with the same representation as the other type.
deriveDeltaOrd :: Name -> Name -> Q [Dec]
deriveDeltaOrd abs rel =
    [d| instance Delta $(conT abs) $(conT rel) where
            delta x y = coerce x - coerce y
            sigma x y = if z >= zero then Just (coerce z) else Nothing
                where z = coerce x + y |]
