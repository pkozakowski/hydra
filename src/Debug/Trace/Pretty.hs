module Debug.Trace.Pretty
    ( module Debug.Trace
    , module Debug.Trace.Pretty
    ) where

import Data.Text.Lazy
import Debug.Trace hiding (traceShow, traceShowM)
import Text.Pretty.Simple

traceShow :: Show a => a -> b -> b
traceShow = trace . unpack . pShow

traceShowA :: (Applicative f, Show a) => a -> f ()
traceShowA = traceM . unpack . pShow 

traceShowM :: (Applicative f, Show a) => a -> f ()
traceShowM = traceShowA
