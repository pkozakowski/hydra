{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Numeric.Delta where

import Data.Coerce
import Numeric.Absolute
import Numeric.Algebra
import Numeric.Relative
import Prelude hiding ((+), (-))

-- | Pairing between an Absolute and a Relative type, which are Modules over the
-- same scalar type. A Relative value represents the difference between two
-- Absolute values. In some cases, it can be integrated back into an Absolute
-- value.
--
class (Absolute a b, Relative a c) => Delta a b c where

    -- | Difference between two Absolutes gives a Relative.
    --
    delta :: b -> b -> c

    -- | Opportunity to reintegrate a Relative into an Absolute. May fail.
    --
    sigma :: b -> c -> Maybe b

-- | Instance for comparable scalars.
--
instance
    ( Ord c
    , Coercible b c
    , Absolute a b
    , Relative a c
    ) => Delta a b c where

    delta x y = coerce x - coerce y
    sigma x y = if z >= zero then Just (coerce z) else Nothing
        where z = coerce x + y
