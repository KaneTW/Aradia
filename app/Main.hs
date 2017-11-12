module Main where

import Control.Lens
import qualified Data.Text as T
import System.Environment

import Aradia
import Aradia.Command.Plex
import Aradia.Types

data AppConfig = AppConfig { _appAradiaConfig :: AradiaConfig
                           , _appPlexConfig :: PlexConfig }

makeLenses ''AppConfig

instance HasAradiaConfig AppConfig where
  aradiaConfig = appAradiaConfig

instance HasPlexConfig AppConfig where
  plexConfig = appPlexConfig

main :: IO ()
main = do
  token <- getEnv "ARADIA_TOKEN"
  ptok <- T.pack <$> getEnv "PLEX_TOKEN"
  psid <- T.pack <$> getEnv "PLEX_SERVER_ID"
  runAradia $ AppConfig { _appAradiaConfig = defaultAradiaConfig { _aradiaToken = token }
                        , _appPlexConfig = PlexConfig { _plexToken = ptok
                                                      , _plexServerId = psid }
                        }
