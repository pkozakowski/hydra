{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
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
    , NoDuplicateIn
    , LabelIn (..)
    , Labels
    , (!)
    , get
    , set
    , getIn
    , setIn
    , labelIn
    , labels
    , fromList
    , toList
    , deriveHomRecord
    , deriveUnary
    , deriveSemimodule
    , deriveModule
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
import Numeric.Algebra
import qualified Numeric.Algebra.Deriving as AlgDeriving
import Numeric.Delta
import Numeric.Kappa
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
    getImpl :: Proxy l -> HomRec ls t -> t
    setImpl :: Proxy l -> t -> HomRec ls t -> HomRec ls t

instance HasAt l (l ': ls) AtHead where
    getImpl _ (_ := x :& _) = x
    setImpl lp x (_ :& r) = lp := x :& r

instance (HasAt l ls i, Where l (l' ': ls) ~ AtTail) => HasAt l (l' ': ls) AtTail where
    getImpl lp (_ :& r) = getImpl lp r
    setImpl lp x (fld :& r) = fld :& setImpl lp x r

-- | The typechecker can't prove this for us, so we cheat a little bit.
membershipMonotonicity :: forall l l' ls. Has l ls :- Has l (l' ': ls)
membershipMonotonicity = unsafeCoerce $ unmapDict $ imply cheat where
    imply
        :: Dict (Where l (l' ': ls) ~ AtTail)
        -> Dict (HasAt l ls i)
        -> Dict (HasAt l (l' ': ls) AtTail)
    imply Dict Dict = Dict
    cheat :: forall a b. Dict (a ~ b)
    cheat = unsafeCoerce (Dict :: forall a. Dict (a ~ a))

-- | Label with a proof of membership.
data LabelIn ls = forall l. KnownSymbol l => LabelIn (Dict (Has l ls))

instance Show (LabelIn ls) where
    show (LabelIn (Dict :: Dict (Has l ls))) = symbolVal (Proxy :: Proxy l)

instance Eq (LabelIn ls) where
    li1 == li2 = show li1 == show li2

labelIn :: forall l ls. (KnownSymbol l, Has l ls) => LabelIn ls
labelIn = LabelIn (Dict :: Dict (Has l ls))

labels :: Labels ls => [LabelIn ls]
labels = fst <$> toList (fill undefined)

class HomRecord ls t r | r -> ls, r -> t where

    get :: Has l ls => Proxy l -> r -> t
    set :: Has l ls => Proxy l -> t -> r -> r
    fromList :: Labels ls => t -> [(LabelIn ls, t)] -> r
    toList :: r -> [(LabelIn ls, t)]

instance HomRecord ls t (HomRec ls t) where

    get = getImpl

    set = setImpl

    fromList def [] = fill def
    fromList def ((li, x) : lixs) = setIn li x $ fromList def lixs

    toList Empty = []
    toList (lp := x :& r)
        = (LabelIn (Dict :: Dict (HasAt l (l ': ls') AtHead)), x) : fmap inj (toList r) where
            inj :: (LabelIn ls', t) -> (LabelIn (l ': ls'), t)
            inj (LabelIn proof, x) = (LabelIn proof', x) where
                proof' = mapDict membershipMonotonicity proof

getIn :: HomRecord ls t r => LabelIn ls -> r -> t
getIn (LabelIn (Dict :: Dict (Has l ls))) = get (Proxy :: Proxy l)

setIn :: HomRecord ls t r => LabelIn ls -> t -> r -> r
setIn (LabelIn (Dict :: Dict (Has l ls))) = set (Proxy :: Proxy l)

(!) :: (Has l ls, HomRecord ls t r) => r -> Proxy l -> t
(!) = flip get

infixl 9 !

type family NoDuplicateIn (l :: u) (ls :: [u]) :: Constraint where
    NoDuplicateIn l '[] = ()
    NoDuplicateIn l (l ': ls) = TypeError (Text "Duplicate key " :<>: ShowType l :<>: Text ".")
    NoDuplicateIn l (_ ': ls) = NoDuplicateIn l ls

class Labels ls where
    fill :: t -> HomRec ls t

instance Labels '[] where
    fill _ = Empty

instance (NoDuplicateIn l ls, KnownSymbol l, Labels ls) => Labels (l ': ls) where
    fill x = Proxy := x :& fill x

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
    show = show . toList

instance (Labels ls, Eq t) => Eq (HomRec ls t) where
    r1 == r2 = all id $ (==) <$> r1 <*> r2

instance (Labels ls, Additive t) => Additive (HomRec ls t) where
    r1 + r2 = (+) <$> r1 <*> r2

instance (Labels ls, Abelian t) => Abelian (HomRec ls t)

instance (Labels ls, Monoidal t) => Monoidal (HomRec ls t) where
    zero = pure zero

instance (Labels ls, Group t) => Group (HomRec ls t) where
    r1 - r2 = (-) <$> r1 <*> r2

instance (Labels ls, LeftModule a t) => LeftModule a (HomRec ls t) where
    n .* r = (n .*) <$> r

instance (Labels ls, RightModule a t) => RightModule a (HomRec ls t) where
    r *. n = (*. n) <$> r

instance (Labels ls, Module a t) => Module a (HomRec ls t)

deriveHomRecord :: Name -> Name -> Q [Dec]
deriveHomRecord t r
    = [d| deriving instance HomRecord ls ($(conT t)) (($(conT r)) ls) |]

deriveUnary :: Name -> [Name] -> Q [Dec]
deriveUnary t cs = do
    (cxt, t') <- requireLabels t
    Deriving.deriveUnary' cxt t' cs

deriveSemimodule :: Name -> Name -> Q [Dec]
deriveSemimodule scr mod = do
    (cxt, t') <- requireLabels mod
    AlgDeriving.deriveSemimodule' cxt scr t'

deriveModule :: Name -> Name -> Q [Dec]
deriveModule scr mod = do
    (cxt, t') <- requireLabels mod
    AlgDeriving.deriveModule' cxt scr t'

requireLabels :: Name -> Q (Cxt, Type)
requireLabels t = do
    ls <- VarT <$> newName "ls"
    return ([AppT (ConT ''Labels) ls], AppT (ConT t) ls)

deriveDelta :: Name -> Name -> Q [Dec]
deriveDelta abs rel = do
    TyConI (NewtypeD _ _ _ _ (NormalC absCons _) _) <- reify abs
    TyConI (NewtypeD _ _ _ _ (NormalC relCons _) _) <- reify rel
    let absP v = conP absCons [varP v]
        relP v = conP relCons [varP v]
    [d| instance Labels ls => Delta ($(conT abs) ls) ($(conT rel) ls) where

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
            kappa $(aP xn) $(bP yn) = fmap $(conE cCons) $ sequenceA $ kappa <$> x <*> y
            kappa' $(aP xn) $(cP yn) = fmap $(conE bCons) $ sequenceA $ kappa' <$> x <*> y
            pi $(bP xn) $(cP yn) = $(conE aCons) $ pi <$> x <*> y |]
    where
        xn = mkName "x"
        yn = mkName "y"
