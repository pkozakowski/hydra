{-# LANGUAGE OverloadedStrings #-}

module Market.Feed.Price where

import Control.Monad
import Data.Bifunctor
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Static
import Data.Maybe
import Data.Text hiding (foldl, scanl, zip)
import Market.Ops
import Market.Types
import Market.Feed
import Market.Feed.MongoDB
import Numeric.Field.Fraction
import Numeric.Precision
import Numeric.Truncatable
import Polysemy
import Polysemy.Error

data PriceFeed

instance FeedType PriceFeed where
    feedName = "price"

runPriceFeed
    :: forall r b
     . Member Precision r
    => [Asset]
    -> (forall a. String -> Sem (Feed Double : r) a -> Sem r a)
    -> Sem (Feed Prices : r) b
    -> Sem r b
runPriceFeed assets interpreter = interpret \case
    Between' from to -> do
        assetToMaybeSeries :: StaticMap Asset (Maybe (TimeSeries Price))
            <- fromList <$> forM assets \asset -> do
                prices <- runPriceFeedForOneToken interpreter (show asset)
                        $ between' @Price @(Feed Price : r) from to
                return (asset, prices)
        return
            $ fmap TimeSeries
            $ nonEmpty
            $ catMaybes
            $ fmap buildTimeStep
            $ scanl update (Nothing, fromList $ zip assets $ repeat Nothing)
            $ sweep
            $ maybe [] seriesToList <$> assetToMaybeSeries
        where
            buildTimeStep (maybeTime, currentPrices)
                = (,) <$> maybeTime <*> (Prices <$> sequenceA currentPrices)
            update (_, currentPrices) (time, Event changes)
                = (Just time, updates currentPrices) where
                    updates
                        = foldl (.) id $ uncurry set . second Just <$> changes

runPriceFeedForOneToken
    :: Member Precision r
    => (forall a. String -> Sem (Feed Double : r) a -> Sem r a)
    -> String
    -> Sem (Feed Price : r) b
    -> Sem r b
runPriceFeedForOneToken interpreter token = interpret \case
    Between' from to
        -> interpreter token (between' @Double from to) >>= \case
            Nothing -> return Nothing
            Just series -> do
                -- Truncate non-monadically to preserve laziness in IO contexts.
                trunc <- getTruncator
                return
                    $ Just
                    $ fmap (Price . runTruncatorReal trunc)
                    $ series
