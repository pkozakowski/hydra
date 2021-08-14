{-# LANGUAGE OverloadedStrings #-}

module Market.Feed.Types where

data PriceVolume = PriceVolume { price :: Double, volume :: Double }
    deriving (Show)
