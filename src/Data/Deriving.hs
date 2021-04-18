{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Deriving where

import Data.Traversable
import Language.Haskell.TH

deriveUnary :: Cxt -> Type -> [Name] -> Q [Dec]
deriveUnary cxt t cs =
    return [StandaloneDerivD Nothing cxt $ AppT (ConT c) t | c <- cs]

deriveBinary :: Cxt -> Name -> Type -> [Name] -> Q [Dec]
deriveBinary cxt t u cs =
    return [StandaloneDerivD Nothing cxt $ AppT (AppT (ConT c) (ConT t)) u | c <- cs]
