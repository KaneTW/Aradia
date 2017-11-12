module Aradia.Command.Plex where

import Control.Lens hiding ((.=))
import Control.Exception.Lifted

import Data.Aeson
import Data.Default
import Data.Monoid
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Trans.Class

import Network.Discord
import Network.HTTP.Req

import Aradia.Command
import Aradia.Types
import Aradia.Utils

data PlexConfig = PlexConfig { _plexToken :: Text
                             , _plexServerId :: Text}
  deriving (Show)

makeClassy ''PlexConfig

data PlexInvite

type instance ConfigFor PlexInvite cfg = HasPlexConfig cfg

instance AradiaCommand PlexInvite where
  commandName _ = "plexinvite"
  commandUsage _ = "<email to invite>"
  commandDescription _ = "sends a plex invite"
  
  handleMessage _ Message{messageChannel = chan} text
    | T.null text = respond chan "Usage: plexinvite <email or username>"
    | otherwise = catch (doPlexRequest text) (\(e :: HttpException) -> respond chan "couldn't invite you, sorry")
    where
      baseUrl = https "plex.tv" /: "api"
      validateUrl = baseUrl /: "users" /: "validate"
      sharedServersUrl sid = baseUrl /: "servers" /: sid /: "shared_servers"
      {-
{"server_id":"551fd6e0243dfda270c5a4eb4ebfd7542a63b5bf","shared_server":{"library_section_ids":[],"invited_email":"bluh"},"sharing_settings":{}}
-}
      authParams token = "X-Plex-Token" =: token

      validateEmail email token = runReq def $
        req POST validateUrl NoReqBody (Proxy :: Proxy LbsResponse) ("invited_email" =: email <> authParams token)

      sendFriendRequest sid email token = runReq def $
        req POST (sharedServersUrl sid) (friendRequestBody sid email) (Proxy :: Proxy LbsResponse) (authParams token)

      -- ad hoc json, don't wanna make a type for this
      friendRequestBody sid email = ReqBodyJson $ 
        object [ "server_id" .= sid
                 , "shared_server"
                   .= object [ "library_section_ids" .= ()
                             , "invited_email" .= email ]
                 , "sharing_settings" .= object [] ]
                                      
      
      doPlexRequest email = do
        token <- lift $ view plexToken
        sid <- lift $ view plexServerId
        _ <- validateEmail email token
        _ <- sendFriendRequest sid email token
        respond chan $ "sent an invite to " `T.append` email `T.append` "!"

          
