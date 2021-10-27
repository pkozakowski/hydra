{-# LANGUAGE TemplateHaskell #-}

module Numeric.Precision where

import Data.Fixed
import Numeric.Field.Fraction
import Numeric.Truncatable
import Polysemy

data Truncator = Truncator
    { runTruncator :: forall a. Truncatable a => a -> a
    , runTruncatorReal :: forall a. Real a => a -> Fraction Integer
    }

data Precision m a where
    GetTruncator  :: Precision m Truncator

makeSem_ ''Precision

truncate
    :: forall a r
     . (Truncatable a, Member Precision r)
    => a
    -> Sem r a
truncate x = do
    truncator <- getTruncator
    return $ runTruncator truncator x

truncateReal
    :: forall a r
     . (Real a, Member Precision r)
    => a
    -> Sem r (Fraction Integer)
truncateReal x = do
    truncator <- getTruncator
    return $ runTruncatorReal truncator x

getTruncator :: Member Precision r => Sem r Truncator

runPrecision :: HasResolution res => res -> Sem (Precision : r) a -> Sem r a
runPrecision res = interpret \case
    GetTruncator -> return Truncator
        { runTruncator = truncateTo res
        , runTruncatorReal = truncateTo res . realToFraction
        }

runPrecisionExact :: Sem (Precision : r) a -> Sem r a
runPrecisionExact = interpret \case
    GetTruncator -> return Truncator
        { runTruncator = id
        , runTruncatorReal = realToFraction
        }

runPrecisionFromTruncator :: Truncator -> Sem (Precision : r) a -> Sem r a
runPrecisionFromTruncator truncator = interpret \case
    GetTruncator -> return truncator
