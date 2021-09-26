{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Market.Internal.IO where

import Control.Exception
import Control.Logging
import Control.Monad
import Control.Retry
import Data.Composition
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text
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

withExponentialBackoff :: forall e a. Exception e => IO a -> IO a
withExponentialBackoff
    = recovering retryPolicy [retryHandler] . const where
        -- Retry with exponential backoff starting from 100ms.
        retryPolicy = exponentialBackoff 100000
        retryHandler
            = logRetries (\(_ :: e) -> return True)
            $ warn . pack .:. defaultLogMsg

cache
    :: IORef (Maybe a)
    -> IO a
    -> Text
    -> IO a
cache cch fetch description = do
    maybeValue <- readIORef cch
    case maybeValue of
        Just value -> do
            debug $ description <> " cached"
            return value
        Nothing -> do
            debug $ description <> " not cached; fetching"
            value <- fetch
            writeIORef cch $ Just value
            return value

cacheF
    :: Ord k
    => IORef (Map k v)
    -> (k -> IO v)
    -> (k -> Text)
    -> k
    -> IO v
cacheF cache fetch describe key = do
    maybeValue <- Map.lookup key <$> readIORef cache
    case maybeValue of
        Just value -> do
            debug $ describe key <> " found in cache"
            return value
        Nothing -> do
            debug $ describe key <> " not found in cache; fetching"
            value <- fetch key
            modifyIORef cache $ Map.insert key value
            return value
