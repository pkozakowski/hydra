{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Record.Hom
    ( HomRec (..)
    , (:=) (..)
    , Has
    , LabelIn (..)
    , Labels
    , (!)
    , get
    , set
    , getIn
    , setIn
    , toList
    , deriveUnary
    , deriveAbsolute
    , deriveRelative
    , deriveDelta
    , deriveKappa
    ) where

import Data.Coerce
import Data.Constraint
import qualified Data.Deriving as Deriving
import Data.Monoid
import Data.Proxy
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

data label := value = Proxy label := value

data HomRec ls t where

    (:&)
        :: (NoDuplicateIn l ls, KnownSymbol l, Labels ls)
        => l := t -> HomRec ls t -> HomRec (l ': ls) t

    Empty :: HomRec '[] t

infix 6 :=
infixr 5 :&

instance l ~ l' => IsLabel l (Proxy l') where
    fromLabel = Proxy

-- | Witness types needed to avoid overlapping instances of Has.
data AtHead
data AtTail

type family Where (l :: u) (ls :: [u]) where
    Where l (l ': _) = AtHead
    Where l (_ ': ls) = AtTail

type Has l ls = HasAt l ls (Where l ls)

class wtn ~ Where l ls => HasAt l ls wtn where
    get :: Proxy l -> HomRec ls t -> t
    set :: Proxy l -> t -> HomRec ls t -> HomRec ls t

instance HasAt l (l ': ls) AtHead where
    get _ (_ := x :& _) = x
    set lp x (_ :& r) = lp := x :& r

instance (HasAt l ls i, Where l (l' ': ls) ~ AtTail) => HasAt l (l' ': ls) AtTail where
    get lp (_ :& r) = get lp r
    set lp x (fld :& r) = fld :& set lp x r

-- | The typechecker can't prove this for us, so we cheat a little bit.
membershipMonotonicity :: Has l ls :- Has l (l' ': ls)
membershipMonotonicity = unmapDict unsafeCoerce

-- | Label with a proof of membership.
data LabelIn ls = forall l. LabelIn (Dict (Has l ls))

getIn :: LabelIn ls -> HomRec ls t -> t
getIn (LabelIn (Dict :: Dict (Has l ls))) = get (Proxy :: Proxy l)

setIn :: LabelIn ls -> t -> HomRec ls t -> HomRec ls t
setIn (LabelIn (Dict :: Dict (Has l ls))) = set (Proxy :: Proxy l)

toList :: HomRec ls t -> [(LabelIn ls, t)]
toList Empty = []
toList (lp := x :& r)
    = (LabelIn (Dict :: Dict (HasAt l (l ': ls) AtHead)), x) : fmap inj (toList r) where
        inj :: (LabelIn ls, t) -> (LabelIn (l ': ls), t)
        inj (LabelIn proof, x) = (LabelIn proof', x) where
            proof' = mapDict membershipMonotonicity proof

(!) :: Has l ls => HomRec ls t -> Proxy l -> t
(!) = flip get

infixl 9 !

type family NoDuplicateIn (l :: u) (ls :: [u]) :: Constraint where
    NoDuplicateIn l '[] = ()
    NoDuplicateIn l (l ': ls) = TypeError (Text "Duplicate key " :<>: ShowType l :<>: Text ".")
    NoDuplicateIn l (_ ': ls) = NoDuplicateIn l ls

class Labels ls where
    fill :: t -> HomRec ls t
    symbolVals :: Proxy ls -> [String]

instance Labels '[] where
    fill _ = Empty
    symbolVals _ = []

instance (NoDuplicateIn l ls, KnownSymbol l, Labels ls) => Labels (l ': ls) where
    fill x = Proxy := x :& fill x
    symbolVals _ = symbolVal (Proxy :: Proxy l) : symbolVals (Proxy :: Proxy ls)

instance Labels ls => Functor (HomRec ls) where
    fmap _ Empty = Empty
    fmap f (lp := x :& r) = lp := f x :& fmap f r

instance Labels ls => Applicative (HomRec ls) where
    pure = fill
    Empty <*> Empty = Empty
    (lp := f :& fr) <*> (_ := x :& xr) = lp := f x :& (fr <*> xr)

instance Labels ls => Foldable (HomRec ls) where
    foldMap _ Empty = mempty
    foldMap f (lp := x :& r) = f x <> foldMap f r

instance Labels ls => Traversable (HomRec ls) where
    sequenceA Empty = pure Empty
    sequenceA (lp := x :& r) = (:&) . (lp :=) <$> x <*> sequenceA r

instance (Labels ls, Show t) => Show (HomRec ls t) where
    show = show . zip (symbolVals (Proxy :: Proxy ls)) . map snd . toList

instance (Labels ls, Additive t) => Additive (HomRec ls t) where
    p1 + p2 = (+) <$> p1 <*> p2

instance (Labels ls, Group t) => Group (HomRec ls t) where
    p1 - p2 = (-) <$> p1 <*> p2

instance (Labels ls, LeftModule a t) => LeftModule a (HomRec ls t) where
    n .* p = (n .*) <$> p

instance (Labels ls, RightModule a t) => RightModule a (HomRec ls t) where
    p *. n = (*. n) <$> p

instance (Labels ls, Monoidal t) => Monoidal (HomRec ls t) where
    zero = pure zero

instance (Labels ls, Module a t) => Module a (HomRec ls t)

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

            delta $(absP xn) $(absP yn) = $(conE relCons) $ delta <$> x <*> y

            sigma $(absP xn) $(relP yn)
                = $(conE absCons) <$> (sequenceA $ sigma <$> x <*> y) |]
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
            kappa $(aP xn) $(bP yn) = $(conE cCons) $ kappa <$> x <*> y
            kappa' $(aP xn) $(cP yn) = $(conE bCons) $ kappa' <$> x <*> y
            pi $(bP xn) $(cP yn) = $(conE aCons) $ pi <$> x <*> y |]
    where
        xn = mkName "x"
        yn = mkName "y"
