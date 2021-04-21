{-# LANGUAGE TemplateHaskell #-}

module Market.Deriving where

import Data.Deriving
import qualified Data.Record.Hom as HR
import Language.Haskell.TH
import Numeric.Absolute
import Numeric.Delta
import Numeric.Relative

deriveQuantityInstances :: Name -> Name -> Name -> Name -> Name -> Q [Dec]
deriveQuantityInstances scr q qd qr qdr = fmap concat $ sequence $
    -- Scalar instances:
    [deriveUnary t [''Eq, ''Ord, ''Show] | t <- [q, qd]] ++ [
        deriveAbsolute scr q,
        deriveRelative scr qd,
        deriveDeltaOrd scr q qd
    ] ++
    -- Record instances:
    [HR.deriveHomRecord t tr | (t, tr) <- [(q, qr), (qd, qdr)]] ++
    [HR.deriveUnary tr [''Show] | tr <- [qr, qdr]] ++ [
        HR.deriveAbsolute scr qr,
        HR.deriveRelative scr qdr,
        HR.deriveDelta scr qr qdr
    ]
