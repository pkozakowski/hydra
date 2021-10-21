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

typeName :: Type -> Name
typeName = \case
    ConT name -> name
    AppT t _ -> typeName t

declareInstance :: Cxt -> Q Type -> [(Name, [Q Pat], Q Exp)] -> Q [Dec]
declareInstance cxt cls methods
    = fmap pure
    $ InstanceD Nothing cxt
        <$> cls
        <*> sequenceA (uncurry3 declareFunction <$> methods)
    where
        uncurry3 f (x, y, z) = f x y z

declareFunction :: Name -> [Q Pat] -> Q Exp -> Q Dec
declareFunction name args body
    = FunD name
        <$> fmap pure 
            ( Clause
                <$> sequenceA args
                <*> (NormalB <$> body)
                <*> pure []
            )
