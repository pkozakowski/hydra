{-# LANGUAGE TemplateHaskell #-}

module Market.Deriving where

import Data.Deriving
import qualified Data.Record.Hom as HR
import Language.Haskell.TH
import Numeric.Absolute
import Numeric.Algebra
import Numeric.Delta
import Numeric.Normalizable
import Numeric.Relative
import Prelude hiding ((+), (*), (/))

deriveQuantityInstances :: Name -> Name -> Name -> Name -> Name -> Q [Dec]
deriveQuantityInstances scr q qd qr qdr = fmap concat $ sequence $
    -- Scalar instances:
    [deriveUnary t [''Eq, ''Ord, ''Show] | t <- [q, qd]] ++ [
        deriveAbsolute scr q,
        deriveRelative scr qd,
        deriveDeltaOrd scr q qd
    ] ++
    -- Record instances:
    [HR.deriveUnary tr [''Show] | tr <- [qr, qdr]] ++ [
        HR.deriveAbsolute scr qr,
        HR.deriveRelative scr qdr,
        HR.deriveDelta scr qr qdr
    ]

-- | Same as above, but without an Absolute instance, as it doesn't make sense to e.g. add two
-- Distributions.
--
deriveDistributionInstances :: Name -> Name -> Name -> Name -> Name -> Q [Dec]
deriveDistributionInstances scr q qd qr qdr = fmap concat $ sequence $
    -- Scalar instances:
    [deriveUnary t [''Eq, ''Ord, ''Show] | t <- [q, qd]] ++ [
        deriveRelative scr qd,
        deriveDeltaOrd scr q qd
    ] ++
    -- Record instances:
    [HR.deriveUnary tr [''Show] | tr <- [qr, qdr]] ++ [
        HR.deriveRelative scr qdr,
        HR.deriveDelta scr qr qdr
    ]

deriveNormalizable :: Name -> Name -> Name -> Name -> Q [Dec]
deriveNormalizable scr d q qr = do
    TyConI (NewtypeD _ _ _ _ (NormalC dCon _) _) <- reify d
    TyConI (NewtypeD _ _ _ _ (NormalC qrCon _) _) <- reify qr
    let qrP = conP qrCon [varP $ mkName "qrn"]
    [d| instance HR.Labels assets =>
            Normalizable ($(conT d) assets) $(conT q) ($(conT qr) assets) where

            norm $(qrP) = foldl (+) zero $ HR.HomRecF qrn

            normalize qs@($(qrP))
                = $(conE dCon) $ HR.unHomRecF $ coerce . div <$> HR.HomRecF qrn where
                    div x = coerce x / coerce n :: $(conT scr)
                    n = norm qs |]

deriveUnnormalizable :: Name -> Name -> Name -> Name -> Q [Dec]
deriveUnnormalizable scr d q qr = do
    TyConI (NewtypeD _ _ _ _ (NormalC dCon _) _) <- reify d
    TyConI (NewtypeD _ _ _ _ (NormalC qrCon _) _) <- reify qr
    [d| instance HR.Labels assets =>
            Unnormalizable ($(conT d) assets) $(conT q) ($(conT qr) assets) where

            unnormalize n $(conP dCon [varP $ mkName "sr"])
                = $(conE qrCon) $ HR.unHomRecF $ coerce . mul <$> HR.HomRecF sr where
                    mul x = coerce n * coerce x :: $(conT scr) |]
