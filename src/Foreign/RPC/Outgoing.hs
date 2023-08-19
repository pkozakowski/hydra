{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module Foreign.RPC.Outgoing
  ( RemoteError (..)
  , RemoteResult (..)
  , Session
  , session
  , call
  , arg
  , result
  ) where

import Control.Concurrent
import Control.Monad
import Data.Aeson qualified as A
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Functor
import Data.MessagePack
import Data.Text
import Data.Text qualified as T
import Df1 qualified
import Dhall (Generic)
import GHC.Generics (Generic)
import GHC.IO.Handle
import Network.MessagePack.Client hiding (call)
import Network.Socket.Free
import Polysemy
import Polysemy.Async
import Polysemy.Error
import Polysemy.Logging (Logging)
import Polysemy.Logging qualified as Log
import Polysemy.Reader
import Polysemy.Resource
import System.Process

type Command = String
type Arg = String
type Port = Int
type Method = String

data SessionData = SessionData
  { processH :: ProcessHandle
  , stderrH :: Handle
  , port :: Port
  }

type Session = Reader SessionData

newSession
  :: Member (Final IO) r
  => Command
  -> (Port -> [Arg])
  -> Sem r SessionData
newSession cmd buildArgs = embedFinal do
  port <- getFreePort
  let args = buildArgs port
  (_, _, Just stderrH, processH) <-
    createProcess
      (proc cmd args)
        { std_out = Inherit
        , std_err = CreatePipe
        }
  threadDelay 3000000
  return SessionData {..}

closeSession :: Member (Final IO) r => SessionData -> Sem r ()
closeSession sess@SessionData {..} =
  embedFinal do
    terminateProcess processH
    cleanupProcess
      ( Nothing
      , Nothing
      , Just stderrH
      , processH
      )

session
  :: Member (Final IO) r
  => Command
  -> (Port -> [Arg])
  -> Sem (Reader SessionData : r) a
  -> Sem r a
session cmd buildArgs m = do
  resourceToIOFinal $
    bracket
      (newSession cmd buildArgs)
      closeSession
      \sess -> raise $ runReader sess m

data RemoteLog = RemoteLog {src :: Text, level :: Df1.Level, msg :: Text}
  deriving (Generic, A.FromJSON)

instance A.FromJSON Df1.Level where
  parseJSON = A.withText "Df1.Level" \text -> case T.unpack text of
    "DEBUG" -> pure Df1.Debug
    "INFO" -> pure Df1.Info
    "WARNING" -> pure Df1.Warning
    "ERROR" -> pure Df1.Error
    _ -> error $ "unrecognized loglevel: " <> T.unpack text

call
  :: Members [Reader SessionData, Error String, Logging, Final IO] r
  => Method
  -> [Object]
  -> Sem r Object
call method args = asyncToIOFinal $ resourceToIOFinal do
  SessionData {..} <- ask
  let forwardLogs = do
        eof <- embedFinal $ hIsEOF stderrH
        unless eof do
          line <- embedFinal $ hGetLine stderrH
          RemoteLog {..} <- fromEither $ A.eitherDecode $ LBS.pack line
          Log.push "rpc" $ Log.attr "src" src $ Log.log level msg
          forwardLogs
  bracket
    (async forwardLogs)
    cancel
    \_ -> embedFinal $ runClient "localhost" port $ rpcCall method args

arg :: MessagePack a => a -> Object
arg = toObject

data RemoteError = RemoteError {type_ :: Text, message :: Text}

type RemoteResult a = Either RemoteError a

result :: MessagePack a => Object -> RemoteResult a
result obj = case parse of
  Success x -> x
  Error err -> error $ "malformed RPC result; error: " <> err <> "; result: " <> show obj
  where
    parse = do
      errObj <- obj .: "error"
      case errObj of
        ObjectNil -> do
          resObj <- obj .: "result"
          pure <$> fromObject resObj
        _ -> do
          ObjectStr type_ <- errObj .: "type"
          ObjectStr message <- errObj .: "message"
          pure $ Left RemoteError {..}
