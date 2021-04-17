{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Numeric.Absolute where

import Language.Haskell.TH
import Numeric.Algebra
import Numeric.Deriving

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
        deriveUnary mod [''Additive, ''Abelian],
        deriveBinary ''Natural mod [''LeftModule, ''RightModule],
        deriveUnary mod [''Monoidal],
        deriveBinary scr mod [''LeftModule, ''RightModule, ''Module, ''Absolute]
    ]
