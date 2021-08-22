{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Market.Time where

import Data.Time
import Polysemy

data Time m a where
    Now :: Time m UTCTime

makeSem ''Time

runTimeConst :: UTCTime -> Sem (Time : r) a -> Sem r a
runTimeConst time = interpret \case
    Now -> return time

runTimeIO :: Member (Embed IO) r => Sem (Time : r) a -> Sem r a
runTimeIO = interpret \case
    Now -> embed getCurrentTime
