{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Numeric.Relative where

import Data.Deriving
import Language.Haskell.TH
import Numeric.Algebra

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

deriveRelative' :: Cxt -> Name -> Type -> Q [Dec]
deriveRelative' cxt scr mod = fmap concat $ sequence [
        deriveUnary cxt mod [''Additive, ''Abelian],
        deriveBinary cxt ''Natural mod [''LeftModule, ''RightModule],
        deriveUnary cxt mod [''Monoidal],
        deriveBinary cxt ''Integer mod [''LeftModule, ''RightModule],
        deriveUnary cxt mod [''Group],
        deriveBinary cxt scr mod [''LeftModule, ''RightModule, ''Relative]
    ]

deriveRelative :: Name -> Name -> Q [Dec]
deriveRelative scr = deriveRelative' [] scr . ConT
