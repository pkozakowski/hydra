{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Market.Asset where

import Control.DeepSeq
import Data.Aeson (FromJSON)
import Data.Bifunctor
import Data.Functor.Contravariant
import Data.String
import Dhall (FromDhall, ToDhall)
import Dhall qualified as Dh
import GHC.Generics
import Test.QuickCheck

newtype Asset = Asset {unAsset :: String}
  deriving (Eq, Generic, Ord)
  deriving newtype (FromJSON, IsString, NFData)

-- NOTE: Those Show and Read instances are different than the newtype-derived ones!
instance Show Asset where
  show = unAsset

-- TODO: Test the roundtrip.
instance Read Asset where
  readsPrec _ = fmap (first Asset) <$> lex

instance FromDhall Asset where
  autoWith _ =
    Dh.record $
      Dh.field "asset" $
        Asset <$> Dh.string

instance ToDhall Asset where
  injectWith _ =
    Dh.recordEncoder $
      unAsset >$< Dh.encodeField "asset"

instance Arbitrary Asset where
  arbitrary = elements testAssets
  shrink = init . testAssetsUpTo . head . unAsset

testAssetsUpTo :: Char -> [Asset]
testAssetsUpTo char = Asset . pure <$> ['A' .. char]

testAssets :: [Asset]
testAssets = testAssetsUpTo 'D'
