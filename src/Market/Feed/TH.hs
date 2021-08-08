{-# LANGUAGE LambdaCase #-}

module Market.Feed.TH where

import Debug.Trace
import Language.Haskell.TH

traceQ :: Ppr a => Q a -> Q a
traceQ monad = do
    result <- monad
    traceM $ pprint result
    return result

removeInstances :: [Name] -> Q [Dec] -> Q [Dec]
removeInstances names decsQ = filter ok <$> decsQ where
    ok = \case
        InstanceD _ _ (AppT _ (ConT name)) _
            -> not $ nameBase name `elem` fmap nameBase names
        _   -> True
