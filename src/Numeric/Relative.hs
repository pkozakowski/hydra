{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Numeric.Relative where

import Language.Haskell.TH
import Numeric.Algebra
import Numeric.Deriving

class
    ( Additive b
    , Abelian b
    , LeftModule Natural b
    , RightModule Natural b
    , Monoidal b
    , LeftModule Integer b
    , RightModule Integer b
    , Group b
    , LeftModule a b
    , RightModule a b
    ) => Relative a b | b -> a

deriveRelative :: Name -> Name -> Q [Dec]
deriveRelative scr mod = fmap concat $ sequence [
        deriveUnary mod [''Additive, ''Abelian],
        deriveBinary ''Natural mod [''LeftModule, ''RightModule],
        deriveUnary mod [''Monoidal],
        deriveBinary ''Integer mod [''LeftModule, ''RightModule],
        deriveUnary mod [''Group],
        deriveBinary scr mod [''LeftModule, ''RightModule, ''Relative]
    ]
