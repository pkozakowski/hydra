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
import Data.Functor
import Data.MessagePack
import Data.Text
import Dhall (Generic)
import GHC.IO.Handle
import Network.MessagePack.Client hiding (call)
import Network.Socket.Free
import Polysemy
import Polysemy.Reader
import Polysemy.Resource
import System.Process
import Test.QuickCheck.State (State (terminal))

type Command = String
type Arg = String
type Port = Int
type Method = String

data SessionData = SessionData
  { processH :: ProcessHandle
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
  (_, _, _, processH) <-
    createProcess
      (proc cmd args)
        { std_out = Inherit
        , std_err = Inherit
        }
  threadDelay 3000000
  return SessionData {..}

closeSession :: Member (Final IO) r => SessionData -> Sem r ()
closeSession sess =
  embedFinal do
    terminateProcess $ processH sess
    cleanupProcess
      ( Nothing
      , Nothing
      , Nothing
      , processH sess
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

call :: Members [Reader SessionData, Final IO] r => Method -> [Object] -> Sem r Object
call method args = do
  SessionData {..} <- ask
  embedFinal $ runClient "localhost" port $ rpcCall method args

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
