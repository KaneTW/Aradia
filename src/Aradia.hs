module Aradia where

import Control.Monad.Reader

import Data.Proxy
import Data.Text
import Network.Discord

import Aradia.Command
import Aradia.Command.Debug
import Aradia.Command.Games
import Aradia.Command.Plex
import Aradia.Types

type AradiaApp =
  MessageCreateEvent :> (Command Ping
                         :<>: Command Ship
                         :<>: Command PlexInvite)

instance ConfigFor AradiaApp cfg => EventHandler AradiaApp (AradiaT cfg IO)

runAradia :: forall cfg. ConfigFor AradiaApp cfg => cfg -> IO ()
runAradia = runReaderT . runAradiaT $ runBot (Proxy :: Proxy (AradiaT cfg IO AradiaApp))
