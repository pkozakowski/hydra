{-# LANGUAGE TemplateHaskell #-}

module Market.Deriving where

import Data.Deriving
import Data.Typeable
import qualified Data.Record.Hom as HR
import Language.Haskell.TH
import Numeric.Algebra
import Numeric.Algebra.Deriving
import Numeric.Delta
import Numeric.Normalizable
import Prelude hiding ((+), (*), (/))

-- | Derives instances of Semimodule, Module and Delta for two pairs of types: scalar and record
-- of absolute and relative quantities. (Semi)modules are over a single, unitless scalar type.
deriveQuantityInstances :: Name -> Name -> Name -> Name -> Name -> Q [Dec]
deriveQuantityInstances scr q qd qr qdr = fmap concat $ sequence $
    -- Scalar instances:
    [deriveUnary t [''Eq, ''Ord, ''Show, ''Typeable] | t <- [q, qd]] ++ [
        deriveSemimodule scr q,
        deriveModule scr qd,
        deriveDeltaOrd q qd
    ] ++
    -- Record instances:
    [HR.deriveUnary tr [''Eq, ''Show, ''Typeable] | tr <- [qr, qdr]] ++ [
        HR.deriveSemimodule scr qr,
        HR.deriveModule scr qdr,
        HR.deriveDelta qr qdr
    ]

-- | Same as above, but without a Semimodule instance, as it doesn't make sense to e.g. add two
-- Distributions.
deriveDistributionInstances :: Name -> Name -> Name -> Name -> Name -> Q [Dec]
deriveDistributionInstances scr q qd qr qdr = fmap concat $ sequence $
    -- Scalar instances:
    [deriveUnary t [''Eq, ''Ord, ''Show] | t <- [q, qd]] ++ [
        deriveModule scr qd,
        deriveDeltaOrd q qd
    ] ++
    -- Record instances:
    [HR.deriveUnary tr [''Eq, ''Show] | tr <- [qr, qdr]] ++ [
        HR.deriveModule scr qdr,
        HR.deriveDelta qr qdr
    ]

deriveNormalizable :: Name -> Name -> Name -> Name -> Q [Dec]
deriveNormalizable scr d q qr = do
    TyConI (NewtypeD _ _ _ _ (NormalC dCon _) _) <- reify d
    TyConI (NewtypeD _ _ _ _ (NormalC qrCon _) _) <- reify qr
    let qrP = conP qrCon [varP $ mkName "qrn"]
    [d| instance HR.Labels assets =>
            Normalizable ($(conT d) assets) $(conT q) ($(conT qr) assets) where

            norm $(qrP) = foldl (+) zero qrn

            normalize qs@($(qrP)) =
                if n /= zero then
                    Just $ $(conE dCon) $ coerce . div <$> qrn
                else
                    Nothing
                where
                    div x = coerce x / coerce n :: $(conT scr)
                    n = norm qs |]

deriveUnnormalizable :: Name -> Name -> Name -> Name -> Q [Dec]
deriveUnnormalizable scr d q qr = do
    TyConI (NewtypeD _ _ _ _ (NormalC dCon _) _) <- reify d
    TyConI (NewtypeD _ _ _ _ (NormalC qrCon _) _) <- reify qr
    [d| instance HR.Labels assets =>
            Unnormalizable ($(conT d) assets) $(conT q) ($(conT qr) assets) where

            unnormalize n $(conP dCon [varP $ mkName "sr"])
                = $(conE qrCon) $ coerce . mul <$> sr where
                    mul x = coerce n * coerce x :: $(conT scr) |]
