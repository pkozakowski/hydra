module Market.Feed.Price where

import Control.Monad
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe
import Data.Record.Hom
import Market.Types
import Market.Feed
import Market.Feed.Types
import Numeric.Field.Fraction
import Numeric.Precision
import Polysemy
import Polysemy.Error

runPriceFeed
    :: forall assets r b
     . (Labels assets, Member Precision r)
    => (forall a. String -> Sem (Feed PriceVolume : r) a -> Sem r a)
    -> Sem (Feed (Prices assets) : r) b
    -> Sem r b
runPriceFeed interpreter = interpret \case
    Between' from to -> do
        assetMaybeSeries :: HomRec assets (Maybe (TimeSeries Price))
            <- fromList Nothing <$> forM (labels @assets) \label -> do
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
            $ maybe [] (NonEmpty.toList . unTimeSeries) <$> assetMaybeSeries
        where
            buildTimeStep (maybeTime, currentPrices)
                = (,) <$> maybeTime <*> (Prices <$> sequenceA currentPrices)
            update (_, currentPrices) (time, Event changes)
                = (Just time, updates currentPrices) where
                    updates
                        = foldl (.) id $ uncurry setIn . mapSnd Just <$> changes
                    mapSnd f (x, y) = (x, f y)

newtype Event assets a = Event { changes :: NonEmpty (LabelIn assets, a) }

sweep :: Labels assets => HomRec assets [TimeStep a] -> [TimeStep (Event assets a)]
sweep assetSeries =
    if all null assetSeries then []
    else (time, Event changes) : sweep assetSeries' where
        time = minimum $ headTime <$> notNullSeries
        changes
            = fromJust $ nonEmpty
            $ labelAndHeadValue <$> filter ((== time) . headTime) notNullSeries
        headTime = fst . head . snd
        labelAndHeadValue (label, series) = (label, snd $ head series)
        notNullSeries = filter (not . null . snd) $ toList assetSeries
        assetSeries' = advance <$> assetSeries where
            advance series = case series of
                [] -> []
                (t, _) : rest
                    | t == time -> rest
                    | otherwise -> series

runPriceFeedForOneToken
    :: Member Precision r
    => (forall a. String -> Sem (Feed PriceVolume : r) a -> Sem r a)
    -> String
    -> Sem (Feed Price : r) b
    -> Sem r b
runPriceFeedForOneToken interpreter token = interpret \case
    Between' from to
        -> interpreter token (between' @PriceVolume from to) >>= \case
            Nothing
                -> return Nothing
            Just series
                -> Just <$> mapM (fmap Price . truncateReal . price) series
