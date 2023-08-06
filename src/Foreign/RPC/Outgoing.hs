module Foreign.RPC (Session, session) where

import Control.Concurrent
import Control.Monad
import Data.Functor
import GHC.IO.Handle
import Network.MessagePack.Client
import Network.Socket.Free
import Polysemy
import Polysemy.Resource
import System.Process

type Command = String
type Arg = String
type Port = Int

data Session = Session
  { processH :: ProcessHandle
  , stdoutH :: Handle
  , stderrH :: Handle
  , port :: Port
  }

new :: Member (Embed IO) r => Command -> [Arg] -> Sem r Session
new cmd args = embed do
  (Nothing, Just stdoutH, Just stderrH, processH) <-
    createProcess
      (proc cmd args)
        { std_out = CreatePipe
        , std_err = CreatePipe
        }
  threadDelay 300000
  return Session {..}

close :: Member (Embed IO) r => Session -> Sem r ()
close sess =
  embed $
    cleanupProcess
      ( Nothing
      , Just $ stdoutH sess
      , Just $ stderrH sess
      , processH sess
      )

session
  :: Member (Embed IO) r => Command -> (Port -> [Arg]) -> Sem (Reader Session : r) a -> Sem r a
session cmd buildArgs m = do
  port <- embed getFreePort
  let args = buildArgs port
  resourceToIO $ bracket (newSession cmd args) closeSession \sess -> runReader sess m

instance (MessagePack o, Member (Reader Session) r) => RpcType (Sem r o) where
  rpcc m args = do
    sess <- ask
    embed $ execClient "localhost" (port sess) $ rpcc m args
