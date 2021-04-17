{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Numeric.Deriving (deriveUnary, deriveBinary) where

import Data.Traversable
import Language.Haskell.TH

deriveUnary :: Name -> [Name] -> Q [Dec]
deriveUnary t cs = concatForM cs $ \c ->
    [d| deriving instance $(conT c) $(conT t) |]

deriveBinary :: Name -> Name -> [Name] -> Q [Dec]
deriveBinary t u cs = concatForM cs $ \c ->
    [d| deriving instance $(appT (conT c) (conT t)) $(conT u) |]

concatForM :: Monad m => [a] -> (a -> m [b]) -> m [b]
concatForM xs = fmap concat . forM xs
