{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Record.Hom
    ( HomRecord
    , HomRec
    , (:=) (..)
    , Has
    , Labels
    , empty
    , (&)
    , get
    , set
    , deriveHomRecord
    , deriveUnary
    , deriveAbsolute
    , deriveRelative
    , deriveDelta
    , deriveKappa
    , HomRecF
    ) where

import Data.Coerce
import qualified Data.Deriving as Deriving
import Data.Kind hiding (Type)
import Data.Monoid
import Data.Proxy
import Data.Type.Equality
import Data.Vinyl hiding ((:~:))
import Data.Vinyl.Functor
import Data.Vinyl.TypeLevel
import GHC.OverloadedLabels
import GHC.TypeLits
import Language.Haskell.TH
import Numeric.Absolute (Absolute, deriveAbsolute')
import Numeric.Algebra
import Numeric.Delta
import Numeric.Kappa
import Numeric.Relative (Relative, deriveRelative')
import Prelude hiding ((+), (-), (*), pi)

newtype HomRec t ls = HomRec (Rec (Const t) ls)

data label := value = Proxy label := value
infix 6 :=

instance l ~ l' => IsLabel (l :: Symbol) (Proxy l') where
    fromLabel = Proxy

type family Has (l :: u) (ls :: [u]) :: Constraint where
    Has l ls = RecElem Rec l l ls ls (RIndex l ls)

type family NoDuplicateIn (l :: u) (ls :: [u]) :: Constraint where
    NoDuplicateIn l '[] = ()
    NoDuplicateIn l (l ': ls) = TypeError (Text "Duplicate key " :<>: ShowType l :<>: Text ".")
    NoDuplicateIn l (_ : ls) = NoDuplicateIn l ls

class HomRecord t (r :: [Symbol] -> *) | r -> t where
    empty :: r '[]
    (&) :: (NoDuplicateIn l ls, KnownSymbol l, Labels ls) => l := t -> r ls -> r (l ': ls)
    get :: Has l ls => Proxy l -> r ls -> t
    set :: Has l ls => Proxy l -> t -> r ls -> r ls

infixr 5 &
infixl 9 !

(!) :: (Has l ls, HomRecord t r) => r ls -> Proxy l -> t
(!) = flip get

instance HomRecord t (HomRec t) where

    empty = HomRec RNil

    (_ := x) & (HomRec r) = HomRec $ Const x :& r

    get :: forall t l ls. Has l ls => Proxy l -> HomRec t ls -> t
    get _ (HomRec r) = getConst $ (rget r :: Const t l)
    set :: forall t l ls. Has l ls
        => Proxy l -> t -> HomRec t ls -> HomRec t ls
    set _ x (HomRec r) = HomRec $ rput (Const x :: Const t l) r

class RecordToList (ls :: [Symbol]) => Labels ls where
    fmapImpl :: (a -> b) -> HomRec a ls -> HomRec b ls
    pureImpl :: a -> HomRec a ls
    apImpl :: HomRec (a -> b) ls -> HomRec a ls -> HomRec b ls
    foldMapImpl :: Monoid m => (a -> m) -> HomRec a ls -> m
    sequenceAImpl :: Applicative f => HomRec (f a) ls -> f (HomRec a ls)
    symbolVals :: Proxy ls -> [String]

instance Labels '[] where
    fmapImpl _ _ = HomRec RNil
    pureImpl _ = HomRec RNil
    apImpl _ _ = HomRec RNil
    foldMapImpl _ _ = mempty
    sequenceAImpl _ = pure $ HomRec RNil
    symbolVals _ = []

instance (NoDuplicateIn l ls, KnownSymbol l, Labels ls) => Labels (l ': ls) where

    fmapImpl f (HomRec (Const x :& r))
        = HomRec $ Const (f x) :& coerce (fmapImpl f (HomRec r))

    pureImpl (x :: t) = HomRec $ Const x :& coerce (pureImpl x :: HomRec t ls)

    apImpl (HomRec (Const f :& fr)) (HomRec (Const x :& xr))
        = HomRec $ Const (f x) :& coerce (HomRec fr `apImpl` HomRec xr)

    foldMapImpl f (HomRec (Const x :& xr)) = f x <> foldMapImpl f (HomRec xr)

    sequenceAImpl (HomRec (Const x :& xr)) = HomRec <$> ((:&) . Const <$> x <*> fxr) where
        fxr = coerce <$> sequenceAImpl (HomRec xr)

    symbolVals _ = symbolVal (Proxy :: Proxy l) : symbolVals (Proxy :: Proxy ls)

instance (Labels ls, Show t) => Show (HomRec t ls) where
    show (HomRec r) = show $ zip (symbolVals (Proxy :: Proxy ls)) $ recordToList r

instance (Labels ls, Additive t) => Additive (HomRec t ls) where
    p1 + p2 = (+) `fmapImpl` p1 `apImpl` p2

instance (Labels ls, Group t) => Group (HomRec t ls) where
    p1 - p2 = (-) `fmapImpl` p1 `apImpl` p2

instance (Labels ls, LeftModule a t) => LeftModule a (HomRec t ls) where
    n .* p = (n .*) `fmapImpl` p

instance (Labels ls, RightModule a t) => RightModule a (HomRec t ls) where
    p *. n = (*. n) `fmapImpl` p

instance (Labels ls, Monoidal t) => Monoidal (HomRec t ls) where
    zero = pureImpl zero

instance (Labels ls, Module a t) => Module a (HomRec t ls)

deriveHomRecord :: Name -> Name -> Q [Dec]
deriveHomRecord elem rec
    = [d| deriving instance HomRecord $(conT elem) $(conT rec) |]

deriveUnary :: Name -> [Name] -> Q [Dec]
deriveUnary t cs = do
    (cxt, t') <- requireLabels t
    Deriving.deriveUnary' cxt t' cs

deriveAbsolute :: Name -> Name -> Q [Dec]
deriveAbsolute scr mod = do
    (cxt, t') <- requireLabels mod
    deriveAbsolute' cxt scr t'

deriveRelative :: Name -> Name -> Q [Dec]
deriveRelative scr mod = do
    (cxt, t') <- requireLabels mod
    deriveRelative' cxt scr t'

requireLabels :: Name -> Q (Cxt, Type)
requireLabels t = do
    ls <- VarT <$> newName "ls"
    return ([AppT (ConT ''Labels) ls], AppT (ConT t) ls)

deriveDelta :: Name -> Name -> Name -> Q [Dec]
deriveDelta scr abs rel = do
    TyConI (NewtypeD _ _ _ _ (NormalC absCons _) _) <- reify abs
    TyConI (NewtypeD _ _ _ _ (NormalC relCons _) _) <- reify rel
    let absP v = conP absCons [varP v]
        relP v = conP relCons [varP v]
    [d| instance Labels ls => Delta $(conT scr) ($(conT abs) ls) ($(conT rel) ls) where

            delta $(absP xn) $(absP yn) = $(conE relCons) $ (delta `fmapImpl` x) `apImpl` y

            sigma $(absP xn) $(relP yn)
                = $(conE absCons) <$> (sequenceAImpl $ sigma `fmapImpl` x `apImpl` y) |]
    where
        xn = mkName "x"
        yn = mkName "y"

deriveKappa :: Name -> Name -> Name -> Q [Dec]
deriveKappa a b c = do
    TyConI (NewtypeD _ _ _ _ (NormalC aCons _) _) <- reify a
    TyConI (NewtypeD _ _ _ _ (NormalC bCons _) _) <- reify b
    TyConI (NewtypeD _ _ _ _ (NormalC cCons _) _) <- reify c
    let aP v = conP aCons [varP v]
        bP v = conP bCons [varP v]
        cP v = conP cCons [varP v]
    [d| instance Labels ls => Kappa ($(conT a) ls) ($(conT b) ls) ($(conT c) ls) where
            kappa $(aP xn) $(bP yn) = $(conE cCons) $ (kappa `fmapImpl` x) `apImpl` y
            kappa' $(aP xn) $(cP yn) = $(conE bCons) $ (kappa' `fmapImpl` x) `apImpl` y
            pi $(bP xn) $(cP yn) = $(conE aCons) $ (pi `fmapImpl` x) `apImpl` y |]
    where
        xn = mkName "x"
        yn = mkName "y"

newtype HomRecF ls t = HomRecF (HomRec t ls)

instance Labels ls => Functor (HomRecF ls) where
    fmap f (HomRecF x) = HomRecF $ fmapImpl f x

instance Labels ls => Applicative (HomRecF ls) where
    pure = HomRecF . pureImpl
    HomRecF f <*> HomRecF x = HomRecF $ f `apImpl` x

instance Labels ls => Foldable (HomRecF ls) where
    foldMap f (HomRecF x) = foldMapImpl f x

instance Labels ls => Traversable (HomRecF ls) where
    sequenceA (HomRecF x) = HomRecF <$> sequenceAImpl x
