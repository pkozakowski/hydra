{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Numeric.Precision where

import Data.Fixed
import Numeric.Truncatable
import Polysemy

data Precision m a where
    Truncate :: Truncatable a => a -> Precision m a

makeSem ''Precision

runPrecision :: HasResolution res => res -> Sem (Precision : r) a -> Sem r a
runPrecision res = interpret \case
    Truncate x -> return $ truncateTo res x
