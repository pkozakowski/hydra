{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Market.Deriving where

import Control.DeepSeq
import Data.Deriving
import Data.Map.Sparse
import Data.Typeable
import Language.Haskell.TH
import Numeric.Algebra
import Numeric.Algebra.Deriving
import Numeric.Delta
import Numeric.Normed
import Numeric.Truncatable
import Market.Asset
import Prelude hiding ((+), (*), (/))

-- | Derives instances of Semimodule, Module, Delta and Truncatable for two
-- pairs of types: scalar and map of absolute and relative quantities.
-- (Semi)modules are over a single, unitless scalar type.
-- Additionally derives ReadMap, BuildMap and SetMap for the map types.
deriveAdditiveQuantityInstances
    :: Name -> Name -> Name -> Name -> Name -> Q [Dec]
deriveAdditiveQuantityInstances scr q qd qm qdm = fmap concat $ sequence
    [ deriveNonAdditiveQuantityInstances scr q qd qm qdm
    , deriveSemimodule scr q
    , deriveSemimodule scr qm
    ]

-- | Same as above, but without Semimodule, as it doesn't make sense to e.g. add
-- two Prices.
deriveNonAdditiveQuantityInstances
    :: Name -> Name -> Name -> Name -> Name -> Q [Dec]
deriveNonAdditiveQuantityInstances scr q qd qm qdm = fmap concat $ sequence $
    [ deriveConstrainedQuantityInstances scr q qd qm qdm
    ] ++
    [ deriveUnary t [''Truncatable] | t <- [q, qd, qm, qdm]
    ] ++
    [ [d| deriving newtype instance $(conT ''SetMap) Asset $(conT v) $(conT tm)
        |]
    | (v, tm) <- [(q, qm), (qd, qdm)]
    ]
    
-- | Same as above, but without SetMap and Truncatable, as setting values
-- arbitrarily may violate some constraints.
deriveConstrainedQuantityInstances
    :: Name -> Name -> Name -> Name -> Name -> Q [Dec]
deriveConstrainedQuantityInstances scr q qd qm qdm = fmap concat $ sequence $
    -- Scalar instances:
    [ deriveUnary t [''Default, ''Eq, ''Ord, ''NFData, ''Show, ''Typeable]
    | t <- [q, qd]
    ] ++
    [ deriveModule scr qd
    , deriveDeltaOrd q qd
    ] ++
    -- Map instances:
    [ deriveUnary tm
        [ ''Eq, ''NFData, ''Show, ''Typeable
        ]
    | tm <- [qm, qdm]
    ] ++
    [ deriveModule scr qdm
    , deriveDeltaNewtype qm qdm
    ] ++
    [ [d| deriving newtype instance $(conT c) Asset $(conT v) $(conT tm) |]
    | c <- [''BuildMap, ''ReadMap]
    , (v, tm) <- [(q, qm), (qd, qdm)]
    ]

deriveNormed :: Name -> Name -> Name -> Name -> Q [Dec]
deriveNormed scr d q qm = do
    TyConI (NewtypeD _ _ _ _ (NormalC dCon _) _) <- reify d
    TyConI (NewtypeD _ _ _ _ (NormalC qmCon _) _) <- reify qm
    let qmP = conP qmCon [varP $ mkName "qmn"]
    [d| instance Normed $(conT d) $(conT q) $(conT qm) where
            norm $(qmP) = foldl (+) zero qmn
            normalize qs@($(qmP)) =
                if n /= zero then
                    Just $ $(conE dCon) $ coerce . div <$> qmn
                else
                    Nothing
                where
                    div x = coerce x / coerce n :: $(conT scr)
                    n = norm qs |]

deriveScalable :: Name -> Name -> Name -> Name -> Q [Dec]
deriveScalable scr d q qm = do
    TyConI (NewtypeD _ _ _ _ (NormalC dCon _) _) <- reify d
    TyConI (NewtypeD _ _ _ _ (NormalC qmCon _) _) <- reify qm
    [d| instance Scalable $(conT d) $(conT q) $(conT qm) where
            scale n $(conP dCon [varP $ mkName "sr"])
                = $(conE qmCon) $ coerce . mul <$> sr where
                    mul x = coerce n * coerce x :: $(conT scr) |]
