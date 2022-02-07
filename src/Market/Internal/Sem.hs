module Market.Internal.Sem where

import Control.Concurrent
import Control.Logging
import Control.Parallel.Strategies
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text
import Data.Time
import Polysemy
import Polysemy.Error

-- | A version of forM for Sem, evaluating elements in parallel. Requires an
-- interpreter-deinterpreter pair.
pforSem
    :: (NFData c, Traversable t)
    => (Sem r b -> Sem '[] c)
    -> (c -> Sem r' b)
    -> t a
    -> (a -> Sem r b)
    -> Sem r' (t b)
pforSem interpreter deinterpreter series action = do
    let results = runEval
            $ parTraversable rdeepseq
            $ run . interpreter . action <$> series
    mapM deinterpreter results

cache
    :: Member (Embed IO) r
    => IORef (Maybe a)
    -> Sem r a
    -> Text
    -> Sem r a
cache cch fetch description = do
    maybeValue <- embed $ readIORef cch
    case maybeValue of
        Just value -> do
            embed $ debug $ description <> " cached"
            return value
        Nothing -> do
            embed $ debug $ description <> " not cached; fetching"
            value <- fetch
            embed $ writeIORef cch $ Just value
            return value

cacheF
    :: (Member (Embed IO) r, Ord k)
    => IORef (Map k v)
    -> (k -> Sem r v)
    -> (k -> Text)
    -> k
    -> Sem r v
cacheF cache fetch describe key = do
    maybeValue <- embed $ Map.lookup key <$> readIORef cache
    case maybeValue of
        Just value -> do
            embed $ debug $ describe key <> " found in cache"
            return value
        Nothing -> do
            embed $ debug $ describe key <> " not found in cache; fetching"
            value <- fetch key
            embed $ modifyIORef cache $ Map.insert key value
            return value

withExponentialBackoff
    :: (Members [Error e, Embed IO] r, Show e)
    => NominalDiffTime -> Int -> (e -> Bool) -> Sem r a -> Sem r a
withExponentialBackoff base repeats predicate action
    = action `catch` \(exc :: e)
   -> if predicate exc && repeats > 0
        then do
            embed
                $ warn
                $ pack (show exc) <> "; waiting " <> pack (show base)
               <> " before retrying (" <> pack (show repeats) <> " retries left)"
            embed $ threadDelay $ floor $ (* 1000000) $ toRational base
            withExponentialBackoff (base * 2) (repeats - 1) predicate action
        else
            throw exc
