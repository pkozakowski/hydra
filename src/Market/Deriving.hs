{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Market.Deriving where

import Control.DeepSeq
import Data.Char
import Data.Deriving
import Data.Map.Sparse
import Data.Text (pack)
import Data.Typeable
import Dhall (FromDhall)
import qualified Dhall as Dh
import Language.Haskell.TH
import Numeric.Algebra
import Numeric.Algebra.Deriving
import Numeric.Delta
import Numeric.Normed
import Numeric.Truncatable
import Market.Asset
import Prelude hiding ((+), (*), (/))

-- | Derives instances of Semimodule, Module, Delta, Truncatable and FromDhall
-- for two pairs of types: scalar and map of absolute and relative quantities.
-- (Semi)modules are over the same, unitless scalar type.
-- Additionally derives ReadMap, BuildMap and SetMap for the map types.
deriveAdditiveQuantityInstances
    :: Name -> Name -> Name -> Type -> Type -> Type -> Q [Dec]
deriveAdditiveQuantityInstances scr q qd k qm qdm = fmap concat $ sequence
    [ deriveNonAdditiveQuantityInstances scr q qd k qm qdm
    , deriveSemimodule scr q
    , deriveSemimodule' [appConT' ''Ord k] scr qm
    ]

-- | Same as above, but without Semimodule, as it doesn't make sense to e.g. add
-- two Prices.
deriveNonAdditiveQuantityInstances
    :: Name -> Name -> Name -> Type -> Type -> Type -> Q [Dec]
deriveNonAdditiveQuantityInstances scr q qd k qm qdm
    = fmap concat $ sequence $
        [ deriveConstrainedQuantityInstances scr q qd k qm qdm
        ] ++
        [ deriveUnary n [''Truncatable] | n <- [q, qd]
        ] ++
        [ deriveUnary' [appConT' ''Ord k] t [''Truncatable] | t <- [qm, qdm]
        ] ++
        [ [d| deriving newtype instance Ord $(pure k)
                => $(conT ''SetMap) $(pure k) $(conT v) $(pure tm)
            |]
        | (v, tm) <- [(q, qm), (qd, qdm)]
        ]
    
-- | Same as above, but without SetMap and Truncatable, as setting values
-- arbitrarily may violate some constraints.
deriveConstrainedQuantityInstances
    :: Name -> Name -> Name -> Type -> Type -> Type -> Q [Dec]
deriveConstrainedQuantityInstances scr q qd k qm qdm
    = fmap concat $ sequence $
        -- Scalar instances:
        [ deriveUnary n
            [ ''Default, ''Eq, ''Ord, ''NFData, ''Show, ''Typeable ]
        | n <- [q, qd]
        ] ++
        [ deriveModule scr qd
        , deriveDeltaOrd q qd
        ] ++
        [ do
            TyConI (NewtypeD _ _ _ _ (NormalC tCon _) _) <- reify t
            let fieldName = pack $ (:) <$> toLower . head <*> tail $ nameBase t
            [d| instance FromDhall $(conT t) where
                    autoWith normalizer
                        = Dh.record
                        $ Dh.field fieldName
                        $ $(conE tCon) <$> Dh.autoWith normalizer
                |]
        | t <- [q, qd]
        ] ++
        -- Map instances:
        [ deriveUnary' [appConT' ''Ord k] tm
            [''Eq]
        | tm <- [qm, qdm]
        ] ++
        [ deriveUnary' [appConT' c k, appConT' ''Ord k] tm [c]
        | tm <- [qm, qdm]
        , c <- [''FromDhall, ''NFData, ''Show, ''Typeable]
        ] ++
        [ deriveModule' [appConT' ''Ord k] scr qdm
        , deriveDeltaNewtype [appConT' ''Ord k] qm qdm
        ] ++
        [ [d| deriving newtype instance Ord $(pure k) =>
                $(conT c) $(pure k) $(conT v) $(pure tm)
            |]
        | c <- [''BuildMap, ''ReadMap]
        , (v, tm) <- [(q, qm), (qd, qdm)]
        ]

deriveNormed :: Name -> Type -> Type -> Type -> Q [Dec]
deriveNormed scr d q qm = do
    TyConI (NewtypeD _ _ _ _ (NormalC dCon _) _) <- reify $ typeName d
    TyConI (NewtypeD _ _ _ _ (NormalC qmCon _) _) <- reify $ typeName qm
    let qmP = conP qmCon [varP $ mkName "qmn"]
    [d| instance Normed $(pure d) $(pure q) $(pure qm) where
            norm $(qmP) = foldl (+) zero qmn
            normalize qs@($(qmP)) =
                if n /= zero then
                    Just $ $(conE dCon) $ coerce . div <$> qmn
                else
                    Nothing
                where
                    div x = coerce x / coerce n :: $(conT scr)
                    n = norm qs
        |]

deriveScalable :: Name -> Type -> Type -> Type -> Q [Dec]
deriveScalable scr d q qm = do
    TyConI (NewtypeD _ _ _ _ (NormalC dCon _) _) <- reify $ typeName d
    TyConI (NewtypeD _ _ _ _ (NormalC qmCon _) _) <- reify $ typeName qm
    [d| instance Scalable $(pure d) $(pure q) $(pure qm) where
            scale n $(conP dCon [varP $ mkName "sr"])
                = $(conE qmCon) $ coerce . mul <$> sr where
                    mul x = coerce n * coerce x :: $(conT scr)
        |]

varKeyT :: Type
varKeyT = VarT $ mkName "key"

appKeyT :: Name -> Type
appKeyT name = AppT (ConT name) varKeyT

appConT :: Name -> Name -> Type
appConT f x = AppT (ConT f) (ConT x)

appConT' :: Name -> Type -> Type
appConT' f x = AppT (ConT f) x
