module Market.Feed.Types where

import Market.Feed
import Polysemy
import Polysemy.Error

data PriceVolume = PriceVolume { price :: Double, volume :: Double }
    deriving (Show)
