{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Numeric.Absolute where

import Data.Foldable
import Data.Traversable
import Language.Haskell.TH
import Numeric.Algebra

class
    ( Additive b
    , Abelian b
    , LeftModule Natural b
    , RightModule Natural b
    , Monoidal b
    , LeftModule a b
    , RightModule a b
    ) => Absolute a b

deriveAbsolute :: Name -> Name -> Q [Dec]
deriveAbsolute scr mod = fmap concat $ sequence [
        deriveUnary [''Additive, ''Abelian],
        deriveBinary ''Natural [''LeftModule, ''RightModule],
        deriveUnary [''Monoidal],
        deriveBinary scr [''LeftModule, ''RightModule, ''Module, ''Absolute]
    ]
    where
        deriveUnary cs = concatMapM cs $ \c ->
            [d| deriving instance $(return $ ConT c)
                    $(return $ ConT mod)
            |]
        deriveBinary scr' cs = concatMapM cs $ \c ->
            [d| deriving instance $(return $ AppT (ConT c) (ConT scr'))
                    $(return $ ConT mod)
            |]
        concatMapM xs = fmap concat . forM xs
