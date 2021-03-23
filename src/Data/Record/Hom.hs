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
    , empty
    , (&)
    , get
    , set
    ) where

import Data.Kind
import Data.Proxy
import Data.Vinyl
import Data.Vinyl.Functor
import Data.Vinyl.TypeLevel
import GHC.OverloadedLabels
import GHC.TypeLits

newtype HomRecord ls t = HomRecord (Rec (Const t) ls)

data label := value = Proxy label := value
infix 6 :=

instance l ~ l' => IsLabel (l :: Symbol) (Proxy l') where
    fromLabel = Proxy

type family Has (l :: u) (ls :: [u]) :: Constraint where
    Has l (l ': ls) = ()
    Has l (_ ': ls) = Has l ls

type family NoDuplicateIn (l :: u) (ls :: [u]) :: Constraint where
    NoDuplicateIn l '[] = ()
    NoDuplicateIn l (l ': ls) = TypeError (Text "Duplicate key " :<>: ShowType l :<>: Text ".")
    NoDuplicateIn l (_ : ls) = NoDuplicateIn l ls

empty :: HomRecord '[] t
empty = HomRecord RNil

(&) :: NoDuplicateIn l ls => l := t -> HomRecord ls t -> HomRecord (l ': ls) t
(_ := x) & (HomRecord r) = HomRecord $ Const x :& r
infixr 5 &

get :: forall t l ls. (RecElem Rec l l ls ls (RIndex l ls), RecElemFCtx Rec (Const t))
    => Proxy l -> HomRecord ls t -> t
get _ (HomRecord r) = getConst $ (rget r :: Const t l)

(!) :: forall t l ls. (RecElem Rec l l ls ls (RIndex l ls), RecElemFCtx Rec (Const t))
    => HomRecord ls t -> Proxy l -> t
(!) = flip get
infixl 9 !

set :: forall t l ls. (RecElem Rec l l ls ls (RIndex l ls), RecElemFCtx Rec (Const t))
    => Proxy l -> t -> HomRecord ls t -> HomRecord ls t
set _ x (HomRecord r) = HomRecord $ rput (Const x :: Const t l) r

instance Functor (HomRecord '[]) where
    fmap _ _ = HomRecord RNil

instance (Functor (HomRecord ls), NoDuplicateIn l ls) => Functor (HomRecord (l ': ls)) where
    fmap f (HomRecord (Const x :& r)) = Proxy := (f x) & fmap f (HomRecord r)

instance Applicative (HomRecord '[]) where
    pure _ = HomRecord RNil
    _ <*> _ = HomRecord RNil

instance (NoDuplicateIn l ls, Applicative (HomRecord ls)) => Applicative (HomRecord (l ': ls)) where
    pure x = Proxy := x & pure x
    HomRecord (Const f :& fr) <*> HomRecord (Const x :& xr) = Proxy := f x & (HomRecord fr <*> HomRecord xr)

class KnownSymbols (ls :: [Symbol]) where
    symbolVals :: Proxy ls -> [String]

instance KnownSymbols '[] where
    symbolVals _ = []

instance (KnownSymbol l, KnownSymbols ls) => KnownSymbols (l ': ls) where
    symbolVals _ = symbolVal (Proxy :: Proxy l) : symbolVals (Proxy :: Proxy ls)

instance (KnownSymbols ls, RecordToList ls, Show t) => Show (HomRecord ls t) where
    show (HomRecord r) = show $ zip (symbolVals (Proxy :: Proxy ls)) $ recordToList r
