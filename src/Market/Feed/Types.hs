{-# LANGUAGE DeriveAnyClass #-}

module Market.Feed.Types
  ( module Data.Fixed
  , FixedScalar
  , Period (..)
  )
where

import Data.Aeson
import Data.Fixed
import Data.MessagePack
import GHC.Generics
import Numeric.Truncatable
import Test.QuickCheck hiding (Fixed)

type FixedScalar = Fixed E6

instance MessagePack FixedScalar where
  toObject = toObject . fractionToFractional @Double . fixedToFraction
  fromObject = fmap (realToFrac @Double) . fromObject

data Period = Second | Minute | Hour | Day
  deriving (Bounded, Enum, Eq, FromJSON, Generic, Ord, ToJSON, Read, Show)

instance Arbitrary Period where
  arbitrary = arbitraryBoundedEnum
