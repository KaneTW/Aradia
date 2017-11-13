module Aradia.Persistent where

import Control.Lens

import Data.Pool
import Database.Persist.Sql

import Network.Discord

import Aradia.Types

data PersistentConfig backend = PersistentConfig { _persistentSqlPool :: Pool backend } 

makeClassy ''PersistentConfig

