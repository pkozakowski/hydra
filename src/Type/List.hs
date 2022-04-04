{-# LANGUAGE TypeFamilies #-}

module Type.List where

type family (as :: [k]) ++ (bs :: [k]) :: [k] where
    '[] ++ bs = bs
    (a : as) ++ bs = a : (as ++ bs)
