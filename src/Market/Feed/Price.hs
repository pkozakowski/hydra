module Market.Feed.Price where

import Control.Monad
import Data.Bifunctor
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Record.Hom
import Market.Ops
import Market.Types
import Market.Feed
import Numeric.Field.Fraction
import Numeric.Precision
import Numeric.Truncatable
import Polysemy
import Polysemy.Error

runPriceFeed
    :: forall assets r b
     . (Labels assets, Member Precision r)
    => (forall a. String -> Sem (Feed Double : r) a -> Sem r a)
    -> Sem (Feed (Prices assets) : r) b
    -> Sem r b
runPriceFeed interpreter = interpret \case
    Between' from to -> do
        assetToMaybeSeries :: Map (LabelIn assets) (Maybe (TimeSeries Price))
            <- Map.fromList <$> forM (labels @assets) \label -> do
                prices <- runPriceFeedForOneToken interpreter (show label)
                        $ between' @Price @(Feed Price : r) from to
                return (label, prices)
        return
            $ fmap TimeSeries
            $ nonEmpty
            $ catMaybes
            $ fmap buildTimeStep
            $ scanl update (Nothing, pure Nothing)
            $ sweep
            $ maybe [] (NonEmpty.toList . unTimeSeries) <$> assetToMaybeSeries
        where
            buildTimeStep (maybeTime, currentPrices)
                = (,) <$> maybeTime <*> (Prices <$> sequenceA currentPrices)
            update (_, currentPrices) (time, Event changes)
                = (Just time, updates currentPrices) where
                    updates
                        = foldl (.) id $ uncurry setIn . second Just <$> changes

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
