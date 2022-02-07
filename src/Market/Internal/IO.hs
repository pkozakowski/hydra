module Market.Internal.IO where

import Control.Exception
import Control.Logging
import Control.Monad
import Control.Retry
import Data.Composition
import Data.Text
import Polysemy
import Polysemy.Error

ioToSem :: Members [Error String, Embed IO] r => IO a -> Sem r a
ioToSem monad = do
    resultOrError <- embed $ Control.Exception.try @IOException monad
    either (Polysemy.Error.throw . show) pure resultOrError

semToIO :: Sem [Error String, Embed IO] a -> IO a
semToIO = either fail return <=< runM . runError

semToIOPure :: Sem '[Error String] a -> IO a
semToIOPure = either fail return . run . runError

withExponentialBackoff :: forall e a. Exception e => IO a -> IO a
withExponentialBackoff
    = recovering retryPolicy [retryHandler] . const where
        -- Retry with exponential backoff starting from 100ms.
        retryPolicy = exponentialBackoff 100000
        retryHandler
            = logRetries (\(_ :: e) -> return True)
            $ warn . pack .:. defaultLogMsg
