module Aradia.Command where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class

import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T

import Network.Discord

import Aradia.Types

data Command a

class AradiaCommand a where
  commandName :: Proxy (Command a) -> Text
  handleMessage :: Proxy (Command a) -> Message -> Text -> DiscordApp (AradiaT IO) ()
  

instance AradiaCommand a => EventMap (Command a) (DiscordApp (AradiaT IO)) where
  type Domain (Command a) = Message
  type Codomain (Command a) = ()

  mapEvent p (m@Message { messageContent = c, messageAuthor = User { userIsBot = isBot } })
    | isBot = mzero
    | otherwise = commandData >>= maybe mzero (handleMessage p m)
    where
      commandData = do
        prefix <- lift $ view aradiaPrefix
        return $ T.stripPrefix (prefix `T.append` commandName p) c


