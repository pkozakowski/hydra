{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Market.Instruments where

import Control.Monad.Trans
--import Data.DateTime
import Data.Proxy
import Data.Record.Hom
import GHC.TypeLits
import Market
import Polysemy
import Polysemy.State

data Hold (held :: Symbol) (released :: Symbol) = Hold

instance (Has held assets, Has released assets, KnownSymbol held, KnownSymbol released)
    => Instrument assets (Hold held released) where

    execute
        :: Members '[State (Hold held released), Market assets] r
        => Prices assets -> Portfolio assets -> Sem r ()
    execute prices portfolio = trade @assets (labelIn @released) (labelIn @held) everything

data Balance balanced = Balance
    { target :: Distribution balanced
    , tolerance :: Scalar
    --, updateEvery :: NominalDiffTime
    --, lastUpdateTime :: UTCTime
    }
