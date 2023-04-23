{-# LANGUAGE DeriveAnyClass #-}

module Market.Feed.Types where

import Data.Aeson
import GHC.Generics
import Test.QuickCheck

data Period = Second | Minute | Hour | Day
  deriving (Bounded, Enum, Eq, FromJSON, Generic, Ord, ToJSON, Read, Show)

instance Arbitrary Period where
  arbitrary = arbitraryBoundedEnum
