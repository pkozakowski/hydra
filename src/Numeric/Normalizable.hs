{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Numeric.Normalizable where

-- | Normalizable vectors.
--
class Unnormalizable d a as => Normalizable d a as | d a -> as, as -> a, as -> d where

    -- | Compute the norm of a vector.
    --
    norm :: as -> a

    -- | Normalize a vector to a unitless distribution.
    --
    normalize :: as -> d

-- | Unnormalizable vectors.
--
class Unnormalizable d a as | d a -> as, as -> a, as -> d where

    -- | Scale a distribution by a norm to get a vector.
    --
    unnormalize :: a -> d -> as
