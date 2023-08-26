{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Market.Feed.IBKR
  ( BarField (..)
  , Contract (..)
  , ContractBarField (..)
  , ContractType (..)
  , Exchange (..)
  , IBKRError (..)
  , allBarFields
  , runFeedIBKR
  ) where

import Control.Exception (assert)
import Control.Monad
import Data.Function
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Class
import Data.Map.Static
import Data.MessagePack
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format.ISO8601 qualified as Time
import Debug.Trace (traceM)
import Debug.Trace.Pretty (traceShowM)
import Foreign.RPC.Outgoing qualified as RPC
import Market.Feed
import Market.Feed.Types
import Market.Types hiding (Value)
import Polysemy
import Polysemy.Embed
import Polysemy.Error
import Polysemy.Logging (Logging)
import Polysemy.Logging qualified as Log
import Polysemy.Reader

data BarField = BidAvg | BidMin | AskAvg | AskMax
  deriving (Show, Eq, Ord, Read)

instance MessagePack BarField where
  toObject = \case
    BidAvg -> ObjectStr "bid_avg"
    BidMin -> ObjectStr "bid_min"
    AskAvg -> ObjectStr "ask_avg"
    AskMax -> ObjectStr "ask_max"

  fromObject = \case
    ObjectStr "bid_avg" -> Success BidAvg
    ObjectStr "bid_min" -> Success BidMin
    ObjectStr "ask_avg" -> Success AskAvg
    ObjectStr "ask_max" -> Success AskMax
    x -> Error $ "malformed BarField: " <> show x

allBarFields :: NonEmpty BarField
allBarFields = [BidAvg, BidMin, AskAvg, AskMax]

data ContractType = Cash
  deriving (Show, Read, Eq, Ord)

instance MessagePack ContractType where
  toObject = \case
    Cash -> ObjectStr "CASH"

  fromObject = \case
    ObjectStr "CASH" -> Success Cash
    x -> Error $ "malformed ContractType: " <> show x

data Exchange = IdealPro
  deriving (Show, Read, Eq, Ord)

instance MessagePack Exchange where
  toObject = \case
    IdealPro -> ObjectStr "IDEALPRO"

  fromObject = \case
    ObjectStr "IDEALPRO" -> Success IdealPro
    x -> Error $ "malformed Exchange: " <> show x

data Contract = Contract
  { symbol :: Text
  , currency :: Text
  , type_ :: ContractType
  , exchange :: Exchange
  }
  deriving (Show, Read, Eq, Ord)

data ContractBarField = ContractBarField {contract :: Contract, barField :: BarField}
  deriving (Show, Read, Eq, Ord)

data IBKRError = ContractNotFound Contract | InvalidPeriod Period | UnexpectedError String
  deriving (Show)

runFeedIBKR
  :: forall r a
   . Members [Error IBKRError, Logging, Final IO] r
  => Sem (Feed (StaticMap ContractBarField FixedScalar) : r) a
  -> Sem r a
runFeedIBKR = interpret \case
  Between_ keys period from to ->
    Log.push "runFeedIBKR" $
      Log.attr "from" (showMinute from) $
        Log.attr "to" (showMinute to) $
          RPC.session
            "poetry"
            ( \port ->
                [ "-C"
                , "python/ibkr"
                , "run"
                , "python"
                , "python/ibkr/ibkr/run.py"
                , "/home/koz4k/dev/tws-api/" -- TODO: parametrize
                , show port
                ]
            )
            do
              when (period /= Minute) $ throw $ InvalidPeriod period
              let groups = NonEmpty.groupBy ((==) `on` contract) keys
                  contracts = contract . NonEmpty.head <$> groups
              contractToMaybeSeries
                :: StaticMap Contract (Maybe (TimeSeries (StaticMap BarField FixedScalar))) <-
                fromList <$> forM contracts \ctr ->
                  (ctr,) <$> betweenForContract ctr from to
              let keyToMaybeSeries =
                    fromList $
                      ( \key ->
                          ( key
                          , fmap (! barField key)
                              <$> (contractToMaybeSeries ! contract key)
                          )
                      )
                        <$> NonEmpty.toList keys
              let merged = mergeFeeds keyToMaybeSeries
              pure $ fmap (remap id) <$> merged
  Between1_ key period from to -> do
    between1_UsingBetween_ runFeedIBKR key period from to

betweenForContract
  :: forall r
   . Members [RPC.Session, Error IBKRError, Logging, Final IO] r
  => Contract
  -> UTCTime
  -> UTCTime
  -> Sem r (Maybe (TimeSeries (StaticMap BarField FixedScalar)))
betweenForContract contract from to = do
  let Contract {..} = contract
  Log.attr "contract" (symbol <> "/" <> currency) do
    maybeContractId :: Maybe Int <-
      Log.mapErrorWithLog UnexpectedError $
        handleResult
          =<< RPC.call
            "fetch_contract_id"
            [ RPC.arg symbol
            , RPC.arg currency
            , RPC.arg type_
            , RPC.arg exchange
            ]
    case maybeContractId of
      Just contractId -> do
        let fromTs :: Int = floor $ utcTimeToPOSIXSeconds from
            toTs :: Int = floor $ utcTimeToPOSIXSeconds to
        timesteps :: [TimeStep (StaticMap BarField FixedScalar)] <-
          Log.mapErrorWithLog UnexpectedError $
            handleResult
              =<< RPC.call
                "fetch_orders_by_minute"
                [ RPC.arg contractId
                , RPC.arg fromTs
                , RPC.arg toTs
                , RPC.arg exchange
                ]
        pure $ seriesFromList timesteps
      Nothing -> throw $ ContractNotFound contract

-- callWithRetry
--  :: ( Members [Reader RPC.SessionData, Error String, Logging, Final IO] r
--     , MessagePack a
--     )
--  => RPC.Method
--  -> [Object]
--  -> Sem r (RPC.RemoteResult a)
-- callWithRetry method args =
--  RPC.call method args
--    `catch` \(_ :: String) -> do
--      Log.warning @String "RPC communication error; retrying"
--      callWithRetry method args

handleResult :: Member (Error String) r => RPC.CallResult a -> Sem r a
handleResult = \case
  Right res -> pure res
  Left err -> case err of
    RPC.RemoteError {..} -> throw $ T.unpack $ "remote " <> type_ <> ": " <> message
    RPC.MalformedResult err -> throw $ "malformed RPC result: " <> T.unpack err

showMinute :: UTCTime -> String
showMinute =
  Time.formatShow $ Time.utcTimeFormat (Time.calendarFormat Time.ExtendedFormat) (Time.hourMinuteFormat Time.ExtendedFormat)
