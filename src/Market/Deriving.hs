{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Market.Deriving where

import Control.DeepSeq
import Data.Deriving
import qualified Data.Record.Hom as HR
import Data.Typeable
import Language.Haskell.TH
import Numeric.Algebra
import Numeric.Algebra.Deriving
import Numeric.Delta
import Numeric.Normalizable
import Numeric.Truncatable
import Prelude hiding ((+), (*), (/))

-- | Derives instances of Semimodule, Module, Delta and Truncatable for two
-- pairs of types: scalar and record of absolute and relative quantities.
-- (Semi)modules are over a single, unitless scalar type.
-- Additionally derives HomRecord for the record types.
deriveQuantityInstances :: Name -> Name -> Name -> Name -> Name -> Q [Dec]
deriveQuantityInstances scr q qd qr qdr = fmap concat $ sequence $
    -- Scalar instances:
    [ deriveUnary t [''Eq, ''Ord, ''NFData, ''Show, ''Typeable, ''Truncatable]
    | t <- [q, qd]
    ] ++ [
        deriveSemimodule scr q,
        deriveModule scr qd,
        deriveDeltaOrd q qd
    ] ++
    -- Record instances:
    [ HR.deriveUnary tr [''Eq, ''NFData, ''Show, ''Typeable, ''Truncatable]
    | tr <- [qr, qdr]
    ] ++ [
        HR.deriveSemimodule scr qr,
        HR.deriveModule scr qdr,
        HR.deriveDelta qr qdr,
        HR.deriveHomRecord q qr,
        HR.deriveHomRecord qd qdr
    ]
    
-- | Same as above, but without:
-- * Semimodule, as it doesn't make sense to e.g. add two
--   Distributions,
-- * HomRecord, because setting values arbitrarily can break the
--   constraint that the elements should sum to either 1 or 0,
-- * Truncatable, for a similar reason.
deriveDistributionInstances :: Name -> Name -> Name -> Name -> Name -> Q [Dec]
deriveDistributionInstances scr q qd qr qdr = fmap concat $ sequence $
    -- Scalar instances:
    [deriveUnary t [''Eq, ''Ord, ''NFData, ''Show, ''Typeable]
    | t <- [q, qd]] ++ [
        deriveModule scr qd,
        deriveDeltaOrd q qd
    ] ++
    -- Record instances:
    [HR.deriveUnary tr [''Eq, ''NFData, ''Show, ''Typeable]
    | tr <- [qr, qdr]] ++ [
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
    [d| instance HR.Labels assets
            => Unnormalizable
                ($(conT d) assets)
                $(conT q)
                ($(conT qr) assets) where

            unnormalize n $(conP dCon [varP $ mkName "sr"])
                = $(conE qrCon) $ coerce . mul <$> sr where
                    mul x = coerce n * coerce x :: $(conT scr) |]
