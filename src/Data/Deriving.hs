{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Deriving where

import Data.Traversable
import Language.Haskell.TH

deriveUnary' :: Cxt -> Type -> [Name] -> Q [Dec]
deriveUnary' cxt t cs =
    return
        [ StandaloneDerivD (Just NewtypeStrategy) cxt
            $ AppT (ConT c) t
        | c <- cs
        ]

deriveUnary :: Name -> [Name] -> Q [Dec]
deriveUnary = deriveUnary' [] . ConT

deriveBinary' :: Cxt -> Name -> Type -> [Name] -> Q [Dec]
deriveBinary' cxt t u cs =
    return
        [ StandaloneDerivD (Just NewtypeStrategy) cxt
            $ AppT (AppT (ConT c) (ConT t)) u
        | c <- cs
        ]

deriveBinary :: Name -> Name -> [Name] -> Q [Dec]
deriveBinary t = deriveBinary' [] t . ConT
