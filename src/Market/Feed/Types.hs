{-# LANGUAGE DeriveAnyClass #-}

module Market.Feed.Types
  ( module Data.Fixed
  , FixedScalar
  , Period (..)
  )
where

import Data.Aeson
import Data.Fixed
import GHC.Generics
import Test.QuickCheck hiding (Fixed)

type FixedScalar = Fixed E6

data Period = Second | Minute | Hour | Day
  deriving (Bounded, Enum, Eq, FromJSON, Generic, Ord, ToJSON, Read, Show)

instance Arbitrary Period where
  arbitrary = arbitraryBoundedEnum
