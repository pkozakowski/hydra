{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Market.Feed.Binance where

import Control.Logging
import Control.Monad
import Data.Aeson
import Data.Function
import Data.Functor.Apply
import Data.IORef
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Text (pack)
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.Vector as Vector
import GHC.Generics
import Market
import Market.Feed
import Market.Internal.IO
import Network.HTTP.Req
import Polysemy
import Polysemy.Error
import qualified System.IO.Lazy as LazyIO
import System.IO.Unsafe

data QuoteAsset = USDT | BTC | BNB
    deriving (Eq, Ord, Read, Show)

allQuoteAssets :: [QuoteAsset]
allQuoteAssets = [USDT, BTC, BNB]

apiUrl :: Url Https
apiUrl = https "api.binance.com" /: "api" /: "v3"

data ExchangeInfo = ExchangeInfo
    { symbols :: [Symbol]
    } deriving (FromJSON, Generic)

data Symbol = Symbol
    { baseAsset :: String
    , quoteAsset :: String
    } deriving (FromJSON, Generic)

fetchQuoteAssets :: IO (Map String QuoteAsset)
fetchQuoteAssets = do
    info
       <- withExponentialBackoff @HttpException
        $ runReq defaultHttpConfig
        $ fmap responseBody 
        $ req GET url NoReqBody jsonResponse mempty
    return
        $ Map.fromList
        $ fmap buildEntry
        $ NonEmpty.groupBy ((==) `on` baseAsset)
        $ sortOn quoteAsset
        $ filter knownQuoteAsset
        $ symbols info
    where
        url = apiUrl /: "exchangeInfo"
        buildEntry (s :| ss) =
            ( baseAsset s
            , minimum $ read . quoteAsset <$> s : ss
            )
        knownQuoteAsset =
            ( (`elem` fmap show allQuoteAssets)
            . quoteAsset
            )

quoteAssetCache :: IORef (Maybe (Map String QuoteAsset))
quoteAssetCache = unsafePerformIO $ newIORef Nothing
{-# NOINLINE quoteAssetCache #-}

cachedFetchQuoteAsset :: String -> IO QuoteAsset
cachedFetchQuoteAsset asset = do
    quoteAssets <- cache quoteAssetCache fetchQuoteAssets "quote assets"
    case Map.lookup asset quoteAssets of
        Just quoteAsset -> return quoteAsset
        Nothing -> fail
            $ "asset "
           ++ asset
           ++ " not found or doesn't have a quote in "
           ++ show allQuoteAssets

newtype Candle = Candle { unCandle :: TimeStep Double }
    deriving (Show)

instance FromJSON Candle where

    parseJSON = withArray "Candle" \elems
        -> fmap Candle $ (,)
            <$> (parseTime  $ elems Vector.! 0)
            <*> (parsePrice $ elems Vector.! 1)
        where
            parseTime = fmap fromTimestampMs . parseJSON where
                fromTimestampMs = posixSecondsToUTCTime . (/ 1000)
            parsePrice = fmap read . parseJSON

fetchSymbolPrices
    :: UTCTime
    -> UTCTime
    -> String
    -> IO (Maybe (TimeSeries Double))
fetchSymbolPrices from to symbol
    = fmap
        ( seriesFromList
        . filter within
        . fmap unCandle
        . concat
        )
    $ LazyIO.run
    $ forM [0, interval .. diffUTCTime to from]
    $ LazyIO.interleave
    . \offset
   -> withExponentialBackoff @HttpException
    $ runReq defaultHttpConfig
    $ fmap responseBody 
    $ req GET url NoReqBody jsonResponse
    $ "symbol" =: symbol
   <> "interval" =: ("1m" :: String)
   <> "limit" =: limit
   <> "startTime" =: toTimestampMs (addUTCTime offset from) where
        within (time, _) = from <= time && time < to
        url = apiUrl /: "klines"
        interval = fromInteger $ limit * 60
        limit = 1000
        toTimestampMs :: UTCTime -> Integer
        toTimestampMs = floor . (1000 *) . utcTimeToPOSIXSeconds

fetchAssetPrices
    :: UTCTime
    -> UTCTime
    -> String
    -> IO (Maybe (TimeSeries Double))
fetchAssetPrices from to asset = do
    quoteAsset <- cachedFetchQuoteAsset asset
    maybeBasePrices <- fetchSymbolPrices from to
        $ asset ++ show quoteAsset
    case quoteAsset of
        USDT -> return maybeBasePrices
        _ -> do
            -- Chain rule.
            maybeQuotePrices <- fetchSymbolPrices from to
                $ show quoteAsset ++ "USDT"
            return do
                basePrices <- maybeBasePrices
                quotePrices <- maybeQuotePrices
                return $ (*) <$> basePrices <.> quotePrices

runPriceFeedBinance
    :: Members [Error String, Embed IO] r
    => String
    -> Sem (Feed Double : r) a
    -> Sem r a
runPriceFeedBinance asset = interpret \case
    Between' from to
        -> ioToSem $ withStderrLogging $ fetchAssetPrices from to asset
