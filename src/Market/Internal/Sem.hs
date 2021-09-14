module Market.Internal.Sem where

import Control.Parallel.Strategies
import Polysemy

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
