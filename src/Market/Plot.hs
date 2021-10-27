
module Market.Plot where

import Data.Bifunctor
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Static
import Data.Text (Text, pack, unpack)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Graphics.Vega.VegaLite
import Market.Evaluation
import Market.Types

defaultBackground :: PropertySpec
defaultBackground = background "rgba(0, 0, 0, 0.03)"

defaultSpecs :: [PropertySpec]
defaultSpecs
  = [ width 800
    , height 400
    , mark Line [MTooltip TTEncoding]
    , defaultBackground
    ]

plotSeries :: TimeSeries Double -> [PropertySpec]
plotSeries series
    = [dt [], enc []] ++ defaultSpecs
    where
        dt = seriesToVega series

        enc = encoding
            . position X [ PName "time", PmType Temporal ]
            . position Y [ PName "value", PmType Quantitative ]

plotTree :: InstrumentTree (TimeSeries Double) -> [PropertySpec]
plotTree tree
    = [dt [], enc []] ++ defaultSpecs
    where
        dt = flatTreeToVega $ substituteKey "" "." $ flattenTree tree

        enc = encoding
            . position X [PName "time", PmType Temporal]
            . position Y [PName "value", PmType Quantitative]
            . color [MName "instrument"]

substituteKey :: Eq k => k -> k -> [(k, v)] -> [(k, v)]
substituteKey key key'
    = fmap $ first $ \k -> if k == key then key' else k

plotEvaluation :: EvaluationOnWindows -> [PropertySpec]
plotEvaluation eval
    = [dt [], enc []] ++ defaultSpecs
    where
        dt = flatTreeToVega' $ metadataSeriesForMetric =<< metrics where
            metrics = fmap fst $ toList $ active eval

        metadataSeriesForMetric metric
            = fmap
                ( first
                $ \(InstrumentName instr)
               -> fromList
                    [   ( "instrument", pack instr )
                    ,   ( "mode"
                        , if instr == "portfolio (active)"
                            then "active"
                            else "passive"
                        )
                    ,   ( "metric", pack $ unMetricName metric )
                    ]
                )
            $ (("portfolio (active)", active eval ! metric) :)
            $ substituteKey "" "portfolio (passive)"
            $ flattenTree
            $ (! metric) <$> passive eval

        enc = encoding
            . position X [PName "time", PmType Temporal]
            . position Y
                [ PName "value"
                , PTitle ""
                , PmType Quantitative
                ]
            . color
                [ MName "instrument"
                , MSort
                    [ CustomSort
                        $ Strings ["portfolio (active)", "portfolio (passive)"]
                    , Ascending
                    ]
                , MScale [SScheme "set1" []]
                ]
            . strokeWidth
                [ MName "mode"
                , MSort [Descending]
                , MScale [SRange $ RMin 1.5]
                ]
            . row [FName "metric"]

seriesToVega :: TimeSeries Double -> [DataColumn] -> Data
seriesToVega (TimeSeries txs)
    = dataFromColumns []
    . dataColumn "time"
        (Numbers $ NonEmpty.toList $ utcToVega . fst <$> txs)
    . dataColumn "value"
        (Numbers $ NonEmpty.toList $ snd <$> txs)

flatTreeToVega
    :: [(InstrumentName, TimeSeries Double)]
    -> [DataColumn]
    -> Data
flatTreeToVega
    = flatTreeToVega'
    . fmap
        ( first
        $ \(InstrumentName name)
            -> fromList [("instrument", pack name)]
        )

flatTreeToVega'
    :: [(StaticMap Text Text, TimeSeries Double)]
    -> [DataColumn]
    -> Data
flatTreeToVega' metadataSeries
    = dataFromColumns []
    . case metadataSeries of
        [] -> id
        (headMetadata, _) : _
           -> dataColumn "time"
                ( Numbers
                $ mapTimeStep (utcToVega . fst) . snd =<< metadataSeries
                )
            . dataColumn "value"
                (Numbers $ mapTimeStep snd . snd =<< metadataSeries)
            . foldl (.) id (metadataColumn . fst <$> toList headMetadata) where
                metadataColumn key
                    = dataColumn key
                    $ Strings
                    $ constAlongSeries (! key) =<< metadataSeries

mapTimeStep :: (TimeStep a -> b) -> TimeSeries a -> [b]
mapTimeStep f = NonEmpty.toList . fmap f . unTimeSeries

constAlongSeries
    :: (a -> b)
    -> (a, TimeSeries c)
    -> [b]
constAlongSeries f = Prelude.replicate
    <$> NonEmpty.length . unTimeSeries . snd
    <*> f . fst

utcToVega :: UTCTime -> Double
utcToVega time
    = fromIntegral $ floor $ utcTimeToPOSIXSeconds time Prelude.* 1000
