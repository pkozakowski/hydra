{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Market.Asset where

import Control.DeepSeq
import Data.String
import GHC.Generics
import Test.QuickCheck

newtype Asset = Asset { unAsset :: String}
    deriving (Eq, Generic, Ord)
    deriving newtype (IsString, NFData, Show)

instance Arbitrary Asset where
    arbitrary = elements testAssets
    shrink = init . testAssetsUpTo . head . unAsset

testAssetsUpTo :: Char -> [Asset]
testAssetsUpTo char = Asset . pure <$> ['A' .. char]

testAssets :: [Asset]
testAssets = testAssetsUpTo 'D'
