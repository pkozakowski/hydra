{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Market.Feed.DB.Types where

import Data.Aeson
import Data.Bifunctor
import Database.Persist.Sql
import Database.Persist.TH
import GHC.Generics
import Market.Feed.Types
import Market.Types
import Numeric.Field.Fraction

newtype PersistScalar = PersistScalar {unPersistScalar :: Scalar}

instance Show PersistScalar where
  show (PersistScalar s) = show (numerator s, denominator s)

instance Read PersistScalar where
  readsPrec p s = first (PersistScalar . uncurry (%)) <$> readsPrec p s

derivePersistField "PersistScalar"

derivePersistField "Period"
