{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Map.Class
    ( BuildMap (..)
    , ReadMap (..)
    , ReadWriteMap (..)
    , SetMap (..)
    , (!)
    , buildMapLaws
    , deriveDeltaNewtype
    , deriveKappaNewtype
    , empty
    , null
    , readMapLaws
    , readWriteMapLaws
    , remap
    , reapplyLeft
    , reapplyOuter
    , reapplyRight
    , setMapLaws
    , showsPrecReadMap
    , size
    ) where

import Data.Bifunctor
import Data.Coerce
import Data.Deriving
import Data.List (elem, nub, sort)
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Traversable.Constrained as Constrained
import GHC.Stack
import Language.Haskell.TH
import Numeric.Algebra hiding ((>))
import Numeric.Delta
import Numeric.Kappa
import Prelude hiding (lookup, null, pi)
import Test.QuickCheck
import Test.QuickCheck.Classes

-- | You can only read.
class ReadMap k v m | m -> k v where
    lookup :: k -> m -> Maybe v
    toList :: m -> [(k, v)]

(!) :: HasCallStack => ReadMap k v m => m -> k -> v
map ! key = case lookup key map of
    Just value -> value
    Nothing -> error "element not in the map"
infixl 9 !

size :: ReadMap k v m => m -> Int
size = length . toList

null :: ReadMap k v m => m -> Bool
null = (0 ==) . size

-- | You can read and write to existing keys.
class ReadMap k v m => SetMap k v m where
    set :: HasCallStack => k -> v -> m -> m

-- | You can read, write, add and remove keys.
class SetMap k v m => ReadWriteMap k v m where
    insert :: k -> v -> m -> m
    delete :: k -> m -> m

class ReadMap k v m => BuildMap k v m where
    fromList :: [(k, v)] -> m

empty :: BuildMap k v m => m
empty = fromList []

instance Ord k => ReadMap k v (Map k v) where
    lookup = Map.lookup
    toList = Map.toList

instance Ord k => SetMap k v (Map k v) where
    set key value = Map.alter f key where
        f = \case
            Just _ -> Just value
            Nothing -> error "element not in the map"

instance Ord k => ReadWriteMap k v (Map k v) where
    insert = Map.insert
    delete = Map.delete

instance Ord k => BuildMap k v (Map k v) where
    fromList = Map.fromList

remap
    :: forall as bs k a b
     . (ReadMap k a as, BuildMap k b bs)
    => (a -> b) -> as -> bs
remap f = fromList . fmap (second f) . toList
infixl 4 `remap`

reapplyLeft
    :: forall fs as bs k a b
     . (HasCallStack, ReadMap k (a -> b) fs, ReadMap k a as, BuildMap k b bs)
    => fs -> as -> bs
reapplyLeft fs as = fromList $ applyEntry <$> toList fs where
    applyEntry (key, fn) = (key, fn $ as ! key)
infixl 4 `reapplyLeft`

reapplyRight
    :: forall fs as bs k a b
     . (HasCallStack, ReadMap k (a -> b) fs, ReadMap k a as, BuildMap k b bs)
    => fs -> as -> bs
reapplyRight fs as = fromList $ applyEntry <$> toList as where
    applyEntry (key, x) = (key, fs ! key $ x)
infixl 4 `reapplyRight`

reapplyOuter
    :: forall fs as bs k a b
     . (HasCallStack, ReadMap k (a -> b) fs, ReadMap k a as, BuildMap k b bs)
    => fs -> as -> bs
reapplyOuter fs as
    = fromList
    $ fmap applyKey 
    $ (fst <$> toList fs) ++ (fst <$> toList as) where
        applyKey key = (key, fs ! key $ as ! key)
infixl 4 `reapplyOuter`

-- Instance utils:

showsPrecReadMap
    :: (Show k, Show v, ReadMap k v m)
    => Int -> m -> ShowS
showsPrecReadMap depth map
    = showParen (depth > 10)
    $ showString "fromList "
    . shows (toList map)

-- | Derive Delta for two newtypes of some maps.
deriveDeltaNewtype :: Cxt -> Type -> Type -> Q [Dec]
deriveDeltaNewtype cxt a b = do
    TyConI (NewtypeD _ _ _ _ (NormalC aCons _) _) <- reify $ typeName a
    TyConI (NewtypeD _ _ _ _ (NormalC bCons _) _) <- reify $ typeName b
    let xp  = conP aCons [varP $ mkName "x" ]
        x'p = conP aCons [varP $ mkName "x'"]
        yp  = conP bCons [varP $ mkName "y" ]

    declareInstance cxt [t| $(conT ''Delta) $(pure a) $(pure b) |]
        [   ( 'delta
            , [xp, x'p]
            , [| $(conE bCons) $ delta <$> x `reapplyOuter` x' |]
            )
        ,   ( 'sigma
            , [xp, yp]
            , [| fmap $(conE aCons)
                    $ Constrained.sequenceA
                    $ sigma <$> x `reapplyOuter` y |]
            )
        ]

-- | Derive Kappa for three newtypes of some maps.
deriveKappaNewtype :: Cxt -> Type -> Type -> Type -> Q [Dec]
deriveKappaNewtype cxt a b c = do
    TyConI (NewtypeD _ _ _ _ (NormalC aCons _) _) <- reify $ typeName a
    TyConI (NewtypeD _ _ _ _ (NormalC bCons _) _) <- reify $ typeName b
    TyConI (NewtypeD _ _ _ _ (NormalC cCons _) _) <- reify $ typeName c
    let xp = conP aCons [varP $ mkName "x"]
        yp = conP bCons [varP $ mkName "y"]
        zp = conP cCons [varP $ mkName "z"]

    declareInstance cxt [t| $(conT ''Kappa) $(pure a) $(pure b) $(pure c) |]
        [   ( 'kappa
            , [xp, yp]
            , [| fmap $(conE cCons)
                    $ Constrained.sequenceA
                    $ kappa <$> x `reapplyOuter` y |]
            )
        ,   ( 'kappa'
            , [xp, zp]
            , [| fmap $(conE bCons)
                    $ Constrained.sequenceA
                    $ kappa' <$> x `reapplyOuter` z |]
            )
        ,   ( 'pi
            , [yp, zp]
            , [| $(conE aCons) $ pi <$> y `reapplyOuter` z |]
            )
        ]

-- Laws:

readMapLaws
    :: forall k v m
     .  ( Arbitrary m
        , Show v, Show m
        , Eq v
        , ReadMap k v m
        )
    => Laws
readMapLaws = Laws "ReadMap"
    [ ("Lookup ToList Agreement", property $ lookupToListAgreement @k @v @m)
    ]

lookupToListAgreement
    :: forall k v m
     . (Show v, Eq v, ReadMap k v m)
    => m -> Property
lookupToListAgreement map
    = conjoin $ (\(key, value) -> lookup key map === Just value) <$> toList map

setMapLaws
    :: forall k v m
     .  ( Arbitrary k, Arbitrary v, Arbitrary m
        , Show k, Show v, Show m
        , Eq v, Eq m
        , SetMap k v m
        )
    => Laws
setMapLaws = Laws "SetMap"
    [ ("Lookup Set Inversion", property $ lookupSetInversion @k @v @m)
    , ("Set Lookup Inversion", property $ setLookupInversion @k @v @m)
    ]

lookupSetInversion
    :: forall k v m
     . (Show v, Eq v, SetMap k v m)
    => k -> v -> m -> Property
lookupSetInversion key value map = case lookup key map of
    Just _ -> lookup key (set key value map) === Just value
    Nothing -> discard

setLookupInversion
    :: forall k v m
     . (Show m, Eq m, SetMap k v m)
    => k -> m -> Property
setLookupInversion key map = case lookup key map of
    Just value -> set key value map === map
    Nothing -> discard

readWriteMapLaws
    :: forall k v m
     .  ( Arbitrary k, Arbitrary v, Arbitrary m
        , Show k, Show v, Show m
        , Eq k, Eq v, Eq m
        , ReadWriteMap k v m
        )
   => Laws
readWriteMapLaws = Laws "ReadWriteMap"
    [ ("Insert Lookup Agreement", property $ insertLookupAgreement @k @v @m)
    , ("Delete ToList Agreement", property $ deleteToListAgreement @k @v @m)
    ]

insertLookupAgreement
    :: forall k v m
     . (Show v, Eq v, ReadWriteMap k v m)
    => k -> v -> m -> Property
insertLookupAgreement key value map
    = lookup key (insert key value map) === Just value

deleteToListAgreement
    :: forall k v m
     . (Show v, Eq k, Eq v, ReadWriteMap k v m)
    => k -> v -> m -> Property
deleteToListAgreement key value map
    = List.lookup key (toList $ delete key map) === Nothing

buildMapLaws
    :: forall k v m
     .  ( Arbitrary k, Arbitrary v, Arbitrary m
        , Show k, Show v, Show m
        , Eq v, Eq m
        , Ord k, Ord v
        , BuildMap k v m
        )
   => Laws
buildMapLaws = Laws "BuildMap"
    [ ("FromList ToList Inversion", property $ fromListToListInversion @k @v @m)
    , ("ToList FromList Inversion", property $ toListFromListInversion @k @v @m)
    ]

fromListToListInversion
    :: forall k v m
     .  ( Arbitrary m
        , Show k, Show v, Show m
        , Eq v, Eq m
        , BuildMap k v m
        )
    => m -> Property
fromListToListInversion map = fromList (toList map) === map

toListFromListInversion
    :: forall k v m
     .  ( Arbitrary m
        , Show k, Show v, Show m
        , Eq v, Eq m
        , Ord k, Ord v
        , BuildMap k v m
        )
    => [(k, v)] -> Property
toListFromListInversion kvs
    = nub keys == keys ==> sort kvs === sort kvs' where
        keys = fst <$> kvs
        kvs' = toList (fromList kvs :: m)
