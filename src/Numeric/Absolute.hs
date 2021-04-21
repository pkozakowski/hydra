{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Numeric.Absolute where

import Data.Deriving
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
    ) => Absolute a b | b -> a

deriveAbsolute' :: Cxt -> Name -> Type -> Q [Dec]
deriveAbsolute' cxt scr mod = fmap concat $ sequence [
        deriveUnary' cxt mod [''Additive, ''Abelian],
        deriveBinary' cxt ''Natural mod [''LeftModule, ''RightModule],
        deriveUnary' cxt mod [''Monoidal],
        deriveBinary' cxt scr mod [''LeftModule, ''RightModule, ''Absolute]
    ]

deriveAbsolute :: Name -> Name -> Q [Dec]
deriveAbsolute scr = deriveAbsolute' [] scr . ConT
