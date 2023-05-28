{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}

module Market.Feed.DB.Types where

import Data.Aeson
import Data.Bifunctor
import Data.Fixed
import Database.Persist.Sql
import Database.Persist.TH
import GHC.Generics
import Market.Feed.Types
import Market.Types
import Numeric.Field.Fraction

newtype PersistScalar = PersistScalar {unPersistScalar :: FixedScalar}
  deriving (Read, Show) via FixedScalar

derivePersistField "PersistScalar"

derivePersistField "Period"
