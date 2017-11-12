module Aradia.Auth where

import Control.Lens
import Control.Monad

import Network.Discord
import Aradia.Types

data GuildOwnerAuth

type instance ConfigFor GuildOwnerAuth cfg = ()

instance HasAradiaConfig cfg => EventMap GuildOwnerAuth (DiscordApp (AradiaT cfg IO)) where
  type Domain GuildOwnerAuth = Message
  type Codomain GuildOwnerAuth = Message

  mapEvent p (m@Message { messageChannel = cid
                        , messageAuthor = User { userId = uid } }) = do
    channel <- doFetch $ GetChannel cid
    case channel of
      Text { channelGuild = gid } -> do
        guild <- doFetch $ GetGuild gid
        if guildOwner guild == uid then return m else mzero
      _ -> mzero

