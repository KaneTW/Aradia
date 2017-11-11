module Aradia where

import Control.Monad.Reader

import Data.Proxy
import Data.Text
import Network.Discord

import Aradia.Command
import Aradia.Command.Debug
import Aradia.Command.Games
import Aradia.Types

type AradiaApp =
  MessageCreateEvent :> (Command Ping :<>: Command Ship)

instance EventHandler AradiaApp (AradiaT IO)

runAradia :: AradiaConfig -> IO ()
runAradia = runReaderT . runAradiaT $ runBot (Proxy :: Proxy (AradiaT IO AradiaApp))
