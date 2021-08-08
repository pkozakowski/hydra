{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Market.Feed.PancakeSwap where

import Control.Exception
import Control.Monad
import Data.Aeson.Types as AesonTypes
import Data.ByteString.Lazy (ByteString)
import Data.Either
import Data.Function
import Data.List
import Data.List.NonEmpty (nonEmpty)
import Data.Monoid (Last(..))
import Data.Morpheus.Client
import Data.Text (Text, pack, unpack)
import Data.Text.Read
import Data.Time
import Data.Time.Clock.POSIX
import Market.Types
import Market.Feed
import Market.Feed.TH
import Network.HTTP.Req
import Polysemy
import Polysemy.Error
import Polysemy.Output
import Prelude hiding (id)
import qualified System.IO.Lazy as LazyIO

newtype BigInt = BigInt { unBigInt :: Integer }
    deriving (Eq, Show, Ord)

instance FromJSON BigInt where
    parseJSON (AesonTypes.String s)
        = either fail return $ BigInt . fst <$> signed decimal s

instance ToJSON BigInt where
    toJSON = AesonTypes.String . pack . show . unBigInt

newtype BigDecimal = BigDecimal { unBigDecimal :: Double }
    deriving (Eq, Show, Ord)

instance FromJSON BigDecimal where
    parseJSON (AesonTypes.String s)
        = either fail return $ BigDecimal . fst <$> double s

instance ToJSON BigDecimal where
    toJSON = AesonTypes.String . pack . show . unBigDecimal

removeInstances ['BigInt, 'BigDecimal] $ defineByDocumentFile
    "data/graphql/pancakeswap_exchange.gql"
    [gql|
        query GetPairsWithPrefix ($prefix: String!) {
            pairsPrefix: pairs (
                where: {
                    name_starts_with: $prefix
                },
                orderBy: trackedReserveBNB,
                orderDirection: desc,
                first: 1
            ) {
                idPrefix: id
                reservePrefix: trackedReserveBNB
            }
        }
    |]

removeInstances ['BigInt, 'BigDecimal] $ defineByDocumentFile
    "data/graphql/pancakeswap_exchange.gql"
    [gql|
        query GetPairsWithSuffix ($suffix: String!) {
            pairsSuffix: pairs (
                where: {
                    name_ends_with: $suffix
                },
                orderBy: trackedReserveBNB,
                orderDirection: desc,
                first: 1
            ) {
                idSuffix: id
                reserveSuffix: trackedReserveBNB
            }
        }
    |]

removeInstances ['BigInt, 'BigDecimal] $ defineByDocumentFile
    "data/graphql/pancakeswap_exchange.gql"
    [gql|
        query GetSwaps(
            $pair: String!,
            $from: BigInt!,
            $to: BigInt!,
            $first: Int!,
            $skip: Int!
        ) {
            swaps (
                where: {
                    pair: $pair,
                    timestamp_gte: $from,
                    timestamp_lt: $to,
                },
                orderBy: timestamp,
                orderDirection: asc, 
                first: $first,
                skip: $skip
            ) {
                timestamp
                amount0In
                amount1In
                amount0Out
                amount1Out
                amountUSD
            }
        }
    |]

pancakeSwapUrl :: Url 'Https
pancakeSwapUrl
    =  https "bsc.streamingfast.io"
    /: "subgraphs"
    /: "name"
    /: "pancakeswap"
    /: "exchange-v2"

fetchFail
    :: (Fetch a, FromJSON a)
    => (ByteString -> IO ByteString) -> Args a -> IO a
fetchFail resolver args = fetch resolver args >>= either fail return

fetchPancakeSwap
    :: (Fetch a, FromJSON a)
    => Args a -> IO a
fetchPancakeSwap = fetchFail $ resolver pancakeSwapUrl

data WhichToken = Token0 | Token1
    deriving (Show)

type TokenInPair = (WhichToken, ID)

fetchStrongestPair :: String -> IO TokenInPair
fetchStrongestPair tokenName = do
    GetPairsWithPrefix asToken0 <- fetchPancakeSwap argsAsToken0
    GetPairsWithSuffix asToken1 <- fetchPancakeSwap argsAsToken1
    if not (null asToken0) && not (null asToken1) then
        if reservePrefix (head asToken0) > reserveSuffix (head asToken1) then
            returnAsToken0 asToken0
        else
            returnAsToken1 asToken1
    else if not $ null asToken0 then
        returnAsToken0 asToken0
    else if not $ null asToken1 then
        returnAsToken1 asToken1
    else
        fail $ "token " ++ tokenName ++ " not found"
    where
        argsAsToken0 = GetPairsWithPrefixArgs { prefix = tokenName ++ "-" }
        argsAsToken1 = GetPairsWithSuffixArgs { suffix = "-" ++ tokenName }
        returnAsToken0 = return . (Token0,) . idPrefix . head
        returnAsToken1 = return . (Token1,) . idSuffix . head

data PriceVolume = PriceVolume { price :: Double, volume :: Double }
    deriving (Show)

fetchPriceVolumes
    :: UTCTime -> UTCTime -> TokenInPair
    -> IO (Maybe (TimeSeries PriceVolume))
fetchPriceVolumes from to (whichToken, pairID) = do
    swaps <- fetchSwaps from to pairID
    return $ fmap TimeSeries $ nonEmpty
           $ aggregateSameTime $ swapToTimeStep <$> swaps
    where
        aggregateSameTime = fmap agg . groupBy sameTime where
            agg timeSteps =
                ( time
                , PriceVolume { price = totalPrice, volume = totalVolume }
                ) where
                    time = fst $ head timeSteps
                    totalPrice = sum (value <$> pvs) / totalVolume
                    totalVolume = sum (volume <$> pvs)
                    value pv = price pv * volume pv
                    pvs = snd <$> timeSteps
            sameTime = (==) `on` fst
        swapToTimeStep swap
            = ( time, PriceVolume { price = price, volume = volume } ) where
                time = posixSecondsToUTCTime $ fromInteger $ unBigInt
                     $ timestamp swap
                price = unBigDecimal (amountUSD swap) / amountToken where
                    amountToken = case whichToken of
                        Token0 -> unBigDecimal (amount0In  swap)
                                + unBigDecimal (amount0Out swap)
                        Token1 -> unBigDecimal (amount1In  swap)
                                + unBigDecimal (amount1Out swap)
                volume = unBigDecimal $ amountUSD swap

fetchSwaps :: UTCTime -> UTCTime -> ID -> IO [SwapsSwap]
fetchSwaps from to id = concat <$> takeWhile (not . null) <$> pages where
    pages = LazyIO.run $ forM [0, 100 ..] $ LazyIO.interleave . \skip -> do
        GetSwaps swaps <- fetchPancakeSwap GetSwapsArgs
            { pair = unpack $ unpackID id
            , from = toTimestamp from
            , to = toTimestamp to
            , first = 100
            , skip = skip
            }
        return swaps
        where
            toTimestamp = BigInt . floor . utcTimeToPOSIXSeconds

resolver :: Url a -> ByteString -> IO ByteString
resolver url body = runReq defaultHttpConfig do
    let headers = header "Content-Type" "application/json"
    responseBody <$> req POST url (ReqBodyLbs body) lbsResponse headers

runPriceVolumeFeedPancakeSwap
    :: Members [Error String, Embed IO] r
    => String
    -> Sem (Feed PriceVolume : r) a
    -> Sem r a
runPriceVolumeFeedPancakeSwap tokenName = interpret \case
    Between' from to -> do
        tokenInPair <- ioToSem $ fetchStrongestPair tokenName
        ioToSem $ fetchPriceVolumes from to tokenInPair
    where
        mapFst f (x, y) = (f x, y)

ioToSem :: Members [Error String, Embed IO] r => IO a -> Sem r a
ioToSem monad = do
    resultOrError <- embed $ Control.Exception.try @IOException monad
    either (Polysemy.Error.throw . show) pure resultOrError
