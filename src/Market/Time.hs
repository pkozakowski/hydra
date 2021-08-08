{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Market.Time where

import Data.Time
import Polysemy

data Time m a where
    Now :: Time m UTCTime

makeSem ''Time

runTime :: UTCTime -> Sem (Time : r) a -> Sem r a
runTime time = interpret \case
    Now -> return time
