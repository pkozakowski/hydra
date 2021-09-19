module Market.Internal.IO where

import Control.Exception
import Control.Monad
import Polysemy
import Polysemy.Error

ioToSem :: Members [Error String, Embed IO] r => IO a -> Sem r a
ioToSem monad = do
    resultOrError <- embed $ Control.Exception.try @IOException monad
    either (Polysemy.Error.throw . show) pure resultOrError

semToIO :: Show e => Sem [Error e, Embed IO] a -> IO a
semToIO = either (fail . show) return <=< runM . runError

semToIOPure :: Show e => Sem '[Error e] a -> IO a
semToIOPure = either (fail . show) return . run . runError
