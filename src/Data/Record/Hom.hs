{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Record.Hom
    ( HomRecord
    , (:=) (..)
    , Has
    , Labels
    , empty
    , (&)
    , get
    , set
    ) where

import Data.Coerce
import Data.Kind
import Data.Proxy
import Data.Vinyl
import Data.Vinyl.Functor
import Data.Vinyl.TypeLevel
import GHC.OverloadedLabels
import GHC.TypeLits
import Numeric.Algebra
import Numeric.Field.Fraction
import Prelude hiding ((+), (*))

newtype HomRecord ls t = HomRecord (Rec (Const t) ls)

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

empty :: HomRecord '[] t
empty = HomRecord RNil

(&) :: Labels (l ': ls) => l := t -> HomRecord ls t -> HomRecord (l ': ls) t
(_ := x) & (HomRecord r) = HomRecord $ Const x :& r
infixr 5 &

get :: forall t l ls. Has l ls => Proxy l -> HomRecord ls t -> t
get _ (HomRecord r) = getConst $ (rget r :: Const t l)

(!) :: Has l ls => HomRecord ls t -> Proxy l -> t
(!) = flip get
infixl 9 !

set :: forall t l ls. Has l ls
    => Proxy l -> t -> HomRecord ls t -> HomRecord ls t
set _ x (HomRecord r) = HomRecord $ rput (Const x :: Const t l) r

class RecordToList (ls :: [Symbol]) => Labels ls where
    fmapImpl :: (a -> b) -> HomRecord ls a -> HomRecord ls b
    pureImpl :: a -> HomRecord ls a
    apImpl :: HomRecord ls (a -> b) -> HomRecord ls a -> HomRecord ls b
    symbolVals :: Proxy ls -> [String]

instance Labels '[] where
    fmapImpl _ _ = HomRecord RNil
    pureImpl _ = HomRecord RNil
    apImpl _ _ = HomRecord RNil
    symbolVals _ = []

instance (NoDuplicateIn l ls, KnownSymbol l, Labels ls) => Labels (l ': ls) where
    fmapImpl f (HomRecord (Const x :& r))
        = HomRecord $ Const (f x) :& coerce (fmap f (HomRecord r))

    pureImpl (x :: t) = HomRecord $ Const x :& coerce (pure x :: HomRecord ls t)

    apImpl (HomRecord (Const f :& fr)) (HomRecord (Const x :& xr))
        = HomRecord $ Const (f x) :& coerce (HomRecord fr `apImpl` HomRecord xr)

    symbolVals _ = symbolVal (Proxy :: Proxy l) : symbolVals (Proxy :: Proxy ls)

instance Labels ls => Functor (HomRecord ls) where
    fmap = fmapImpl

instance Labels ls => Applicative (HomRecord ls) where
    pure = pureImpl
    (<*>) = apImpl

instance (Labels ls, Show t) => Show (HomRecord ls t) where
    show (HomRecord r) = show $ zip (symbolVals (Proxy :: Proxy ls)) $ recordToList r

instance (Labels ls, Additive t) => Additive (HomRecord ls t) where
    p1 + p2 = (+) <$> p1 <*> p2

instance (Labels ls, LeftModule a t) => LeftModule a (HomRecord ls t) where
    n .* p = (n .*) <$> p

instance (Labels ls, RightModule a t) => RightModule a (HomRecord ls t) where
    p *. n = (*. n) <$> p

instance (Labels ls, Monoidal t) => Monoidal (HomRecord ls t) where
    zero = pure zero

instance (Labels ls, Module a t) => Module a (HomRecord ls t)
