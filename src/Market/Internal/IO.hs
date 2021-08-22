module Market.Internal.IO where

import Control.Exception
import Control.Monad
import Polysemy
import Polysemy.Error

ioToSem :: Members [Error String, Embed IO] r => IO a -> Sem r a
ioToSem monad = do
    resultOrError <- embed $ Control.Exception.try @IOException monad
    either (Polysemy.Error.throw . show) pure resultOrError

semToIO :: Sem [Error String, Embed IO] a -> IO a
semToIO = runM . runError >=> either fail return
