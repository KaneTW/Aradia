module Aradia where

import Control.Monad.Reader

import Data.Proxy
import Data.Text
import Network.Discord

import Aradia.Command
import Aradia.Command.Debug
import Aradia.Types

type AradiaApp =
  MessageCreateEvent :> Command Ping

instance EventHandler AradiaApp (AradiaT IO)

runAradia :: AradiaConfig -> IO ()
runAradia = runReaderT . runAradiaT $ runBot (Proxy :: Proxy (AradiaT IO AradiaApp))
