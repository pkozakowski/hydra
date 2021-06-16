{-# LANGUAGE TemplateHaskell #-}

module Numeric.Algebra.Deriving where

import Data.Deriving
import Language.Haskell.TH
import Numeric.Algebra

-- | Semimodule over a semiring is a commutative monoid.
deriveSemimodule' :: Cxt -> Name -> Type -> Q [Dec]
deriveSemimodule' cxt scr mod = fmap concat $ sequence [
        deriveUnary' cxt mod [''Additive, ''Abelian],
        deriveBinary' cxt ''Natural mod [''LeftModule, ''RightModule, ''Module],
        deriveUnary' cxt mod [''Monoidal],
        deriveBinary' cxt scr mod [''LeftModule, ''RightModule, ''Module]
    ]

deriveSemimodule :: Name -> Name -> Q [Dec]
deriveSemimodule scr = deriveSemimodule' [] scr . ConT

-- | Module over a ring is an abelian group.
deriveModule' :: Cxt -> Name -> Type -> Q [Dec]
deriveModule' cxt scr mod = fmap concat $ sequence [
        deriveUnary' cxt mod [''Additive, ''Abelian],
        deriveBinary' cxt ''Natural mod [''LeftModule, ''RightModule, ''Module],
        deriveUnary' cxt mod [''Monoidal],
        deriveBinary' cxt ''Integer mod [''LeftModule, ''RightModule, ''Module],
        deriveUnary' cxt mod [''Group],
        deriveBinary' cxt scr mod [''LeftModule, ''RightModule, ''Module]
    ]

deriveModule :: Name -> Name -> Q [Dec]
deriveModule scr = deriveModule' [] scr . ConT
