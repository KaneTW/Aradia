module Aradia.Command.Debug where

import Control.Monad

import qualified Data.Text as T
import Network.Discord

import Aradia.Command
import Aradia.Types

data Ping

instance AradiaCommand Ping where
  commandName _ = "ping"
  handleMessage _ Message{messageChannel = chan} text
    = void . doFetch $ CreateMessage chan ("pong! you wrote ```\n" `T.append` text `T.append` "\n```") Nothing

