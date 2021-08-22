{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Market.Feed.PancakeSwap where

import Control.Exception
import Control.Logging
import Control.Monad
import Data.Aeson.Types as AesonTypes
import Data.ByteString.Lazy (ByteString)
import Data.Either
import Data.Function
import Data.Hashable
import qualified Data.HashMap as HashMap
import Data.IORef
import Data.List
import Data.List.NonEmpty (nonEmpty)
import Data.Monoid (Last(..))
import Data.Morpheus.Client
import Data.Text (Text, pack, unpack)
import Data.Text.Read
import Data.Time
import Data.Time.Clock.POSIX
import GHC.Generics (Generic)
import Market.Internal.IO
import Market.Types
import Market.Feed
import Market.Feed.TH
import Market.Feed.Types
import Network.HTTP.Req
import Polysemy
import Polysemy.Error
import Polysemy.Output
import Prelude hiding (id)
import System.IO.Unsafe
import qualified System.IO.Lazy as LazyIO

newtype BigInt = BigInt { unBigInt :: Integer }
    deriving (Eq, Show, Ord)

instance FromJSON BigInt where
    parseJSON (AesonTypes.String s)
        = either fail return $ BigInt . fst <$> signed decimal s
    parseJSON _ = fail "invalid JSON type; expected a string"

instance ToJSON BigInt where
    toJSON = AesonTypes.String . pack . show . unBigInt

newtype BigDecimal = BigDecimal { unBigDecimal :: Double }
    deriving (Eq, Show, Ord)

deriving newtype instance Num BigDecimal

instance FromJSON BigDecimal where
    parseJSON (AesonTypes.String s)
        = either fail return $ BigDecimal . fst <$> double s
    parseJSON _ = fail "invalid JSON type; expected a string"

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
    :: (Fetch a, FromJSON a, Show (Args a))
    => Args a -> IO a
fetchPancakeSwap args = do
    debug $ "fetching from PancakeSwap: " <> pack (show args)
    fetchFail (resolver pancakeSwapUrl) args

data WhichToken = Token0 | Token1
    deriving (Generic, Show, Eq, Ord, Hashable)

deriving instance Ord ID

type TokenInPair = (WhichToken, ID)

cachedFetchIORef
    :: (Hashable k, Ord k)
    => IORef (HashMap.Map k v)
    -> (k -> IO v)
    -> (k -> Text)
    -> k
    -> IO v
cachedFetchIORef cache fetch describe key = do
    maybeValue <- HashMap.lookup key <$> readIORef cache
    case maybeValue of
        Just value -> do
            debug $ describe key <> " found in cache"
            return value
        Nothing -> do
            debug $ describe key <> " not found in cache; fetching"
            value <- fetch key
            modifyIORef cache $ HashMap.insert key value
            return value

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

pairCache :: IORef (HashMap.Map String TokenInPair)
pairCache = unsafePerformIO $ newIORef HashMap.empty
{-# NOINLINE pairCache #-}

cachedFetchStrongestPair :: String -> IO TokenInPair
cachedFetchStrongestPair
    = cachedFetchIORef pairCache fetchStrongestPair
    $ \tokenName -> "strongest pair for token " <> pack tokenName

fetchPriceVolumes
    :: UTCTime -> UTCTime -> TokenInPair
    -> IO (Maybe (TimeSeries PriceVolume))
fetchPriceVolumes from to (whichToken, pairID) = do
    swaps <- fetchSwaps from to pairID
    return
        $ fmap TimeSeries
        $ nonEmpty
        $ aggregateSameTime
        $ fmap swapToTimeStep
        $ filter isValid swaps
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
        isValid swap
            = amount0In swap + amount0Out swap > BigDecimal 0.0
           && amount1In swap + amount1Out swap > BigDecimal 0.0

fetchSwaps :: UTCTime -> UTCTime -> ID -> IO [SwapsSwap]
fetchSwaps from to id
    = fmap concat
    $ LazyIO.run
    -- Iterate first over short time intervals...
    $ forM [0, interval .. diffUTCTime to from] 
    $ LazyIO.interleave
    . \offset
   -> fmap (concat . takeWhile (not . null))
    $ LazyIO.run
    -- ... then over pages of the result.
    -- We can't do this in one loop because there's a limit for "skip".
    $ forM [0, pageSize ..]
    $ LazyIO.interleave
    . \skip -> do
        let args = GetSwapsArgs
                { pair = unpack $ unpackID id
                , from = toTimestamp $ addUTCTime offset from
                , to = toTimestamp
                     $ min to
                     $ addUTCTime (offset + interval) from
                , first = pageSize
                , skip = skip
                }
        GetSwaps swaps <- fetchPancakeSwap args
        return swaps
        where
            interval = fromInteger 600
            pageSize = 100
            toTimestamp = BigInt . floor . utcTimeToPOSIXSeconds

fetchBeginTime :: TokenInPair -> IO (Maybe UTCTime)
fetchBeginTime (_, pairID) = do
    now <- getCurrentTime
    let args = GetSwapsArgs
            { pair = unpack $ unpackID pairID
            , from = BigInt 0
            , to = BigInt $ floor $ utcTimeToPOSIXSeconds now
            , first = 1
            , skip = 0
            }
    GetSwaps swaps <- fetchPancakeSwap args
    if null swaps then
        return Nothing
    else
        return $ Just $ posixSecondsToUTCTime $ fromInteger
               $ unBigInt $ timestamp $ head swaps

beginTimeCache :: IORef (HashMap.Map TokenInPair (Maybe UTCTime))
beginTimeCache = unsafePerformIO $ newIORef HashMap.empty
{-# NOINLINE beginTimeCache #-}

cachedFetchBeginTime :: TokenInPair -> IO (Maybe UTCTime)
cachedFetchBeginTime
    = cachedFetchIORef beginTimeCache fetchBeginTime
    $ \(_, pairID) -> "begin time for pair " <> pack (show pairID)

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
    Between' from to -> ioToSem $ withStderrLogging do
        tokenInPair <- cachedFetchStrongestPair tokenName
        cachedFetchBeginTime tokenInPair >>= \case
            Just beginTime -> do
                let from' = max from beginTime
                fetchPriceVolumes from' to tokenInPair
            Nothing -> return Nothing
    where
        mapFst f (x, y) = (f x, y)
