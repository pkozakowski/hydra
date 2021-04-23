{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
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
    ( HomRec
    , (:=) (..)
    , Has
    , LabelIn (..)
    , Labels
    , empty
    , (&)
    , (!)
    , get
    , set
    , toList
    , deriveUnary
    , deriveAbsolute
    , deriveRelative
    , deriveDelta
    , deriveKappa
    , HomRecF (..)
    ) where

import Data.Coerce
import Data.Constraint
import qualified Data.Deriving as Deriving
import Data.Kind hiding (Type)
import Data.Monoid
import Data.Proxy
import Data.Type.Equality
import Data.Vinyl hiding ((:~:), Dict)
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
import Unsafe.Coerce

newtype HomRec t ls = HomRec (Rec (Const t) ls)

data label := value = Proxy label := value
infix 6 :=

instance l ~ l' => IsLabel (l :: Symbol) (Proxy l') where
    fromLabel = Proxy

-- | Witness types needed to avoid overlapping instances of Has.
data AtHead
data AtTail

type family Where (l :: u) (ls :: [u]) where
    Where l (l ': _) = AtHead
    Where l (_ ': ls) = AtTail

type Has l ls = HasAt l ls (Where l ls)

class wtn ~ Where l ls => HasAt l ls wtn where
    get :: Proxy l -> HomRec t ls -> t
    set :: Proxy l -> t -> HomRec t ls -> HomRec t ls

instance HasAt l (l ': ls) AtHead where

    get _ (HomRec (Const x :& _)) = x
    set _ x (HomRec (_ :& xr)) = HomRec $ Const x :& xr

instance (HasAt l ls i, Where l (l' ': ls) ~ AtTail) => HasAt l (l' ': ls) AtTail where

    get lp (HomRec (_ :& xr)) = get lp $ HomRec xr
    set lp x (HomRec (y :& yr)) = HomRec $ y :& coerce (set lp x $ HomRec yr)

-- | The typechecker can't prove this for us, so we cheat a little bit.
membershipMonotonicity :: Has l ls :- Has l (l' ': ls)
membershipMonotonicity = unmapDict unsafeCoerce

type family NoDuplicateIn (l :: u) (ls :: [u]) :: Constraint where
    NoDuplicateIn l '[] = ()
    NoDuplicateIn l (l ': ls) = TypeError (Text "Duplicate key " :<>: ShowType l :<>: Text ".")
    NoDuplicateIn l (_ ': ls) = NoDuplicateIn l ls

-- | Label with a membership proof.
data LabelIn ls = forall l. LabelIn (Dict (Has l ls))

infixr 5 &
infixl 9 !

(!) :: Has l ls => HomRec t ls -> Proxy l -> t
(!) = flip get

empty :: HomRec t '[]
empty = HomRec RNil

(&) :: (NoDuplicateIn l ls, KnownSymbol l, Labels ls)
    => l := t -> HomRec t ls -> HomRec t (l ': ls)
_ := x & HomRec r = HomRec $ Const x :& r

class RecordToList (ls :: [Symbol]) => Labels ls where
    fmapImpl :: (a -> b) -> HomRec a ls -> HomRec b ls
    pureImpl :: a -> HomRec a ls
    apImpl :: HomRec (a -> b) ls -> HomRec a ls -> HomRec b ls
    foldMapImpl :: Monoid m => (a -> m) -> HomRec a ls -> m
    sequenceAImpl :: Applicative f => HomRec (f a) ls -> f (HomRec a ls)
    symbolVals :: Proxy ls -> [String]
    toList :: HomRec t ls -> [(LabelIn ls, t)]

instance Labels '[] where
    fmapImpl _ _ = HomRec RNil
    pureImpl _ = HomRec RNil
    apImpl _ _ = HomRec RNil
    foldMapImpl _ _ = mempty
    sequenceAImpl _ = pure $ HomRec RNil
    symbolVals _ = []
    toList _ = []

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

    toList (HomRec (Const x :& xr))
        = (LabelIn (Dict :: Dict (HasAt l (l ': ls) AtHead)), x)
        : fmap inj (toList $ HomRec xr)
        where
            inj :: (LabelIn ls, t) -> (LabelIn (l ': ls), t)
            inj (LabelIn (proof :: Dict (Has l' ls)), x) = (LabelIn proof', x) where
                proof' = mapDict membershipMonotonicity proof

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

newtype HomRecF ls t = HomRecF { unHomRecF :: HomRec t ls }

instance Labels ls => Functor (HomRecF ls) where
    fmap f (HomRecF x) = HomRecF $ fmapImpl f x

instance Labels ls => Applicative (HomRecF ls) where
    pure = HomRecF . pureImpl
    HomRecF f <*> HomRecF x = HomRecF $ f `apImpl` x

instance Labels ls => Foldable (HomRecF ls) where
    foldMap f (HomRecF x) = foldMapImpl f x

instance Labels ls => Traversable (HomRecF ls) where
    sequenceA (HomRecF x) = HomRecF <$> sequenceAImpl x
