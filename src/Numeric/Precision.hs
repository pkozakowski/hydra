{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Numeric.Precision where

import Data.Fixed
import Numeric.Field.Fraction
import Numeric.Truncatable
import Polysemy

data Precision m a where
    Truncate :: Truncatable a => a -> Precision m a
    TruncateReal :: Real a => a -> Precision m (Fraction Integer)

makeSem_ ''Precision

truncate
    :: forall a r
     . (Truncatable a, Member Precision r)
    => a
    -> Sem r a

truncateReal
    :: forall a r
     . (Real a, Member Precision r)
    => a
    -> Sem r (Fraction Integer)

runPrecision :: HasResolution res => res -> Sem (Precision : r) a -> Sem r a
runPrecision res = interpret \case
    Truncate x -> return $ truncateTo res x
    TruncateReal x -> return $ truncateTo res $ realToFraction x

runPrecisionExact :: Sem (Precision : r) a -> Sem r a
runPrecisionExact = interpret \case
    Truncate x -> return x
    TruncateReal x -> return $ realToFraction x
