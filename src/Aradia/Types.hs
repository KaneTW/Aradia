{-# LANGUAGE UndecidableInstances #-}
module Aradia.Types where

import Control.Applicative
import Control.Lens

import Control.Monad.Base
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Text
import GHC.Exts

import Network.Discord


data AradiaConfig = AradiaConfig { _aradiaToken :: String
                                 , _aradiaPrefix :: Text }
  deriving (Show)
makeClassy ''AradiaConfig

defaultAradiaConfig :: AradiaConfig
defaultAradiaConfig = AradiaConfig { _aradiaToken = error "You must define a token"
                                   , _aradiaPrefix = ">" }

-- |Used to construct the set of constraints the given config data type has to fulfill
type family ConfigFor app cfg :: Constraint
type instance ConfigFor (a :> b) cfg = (ConfigFor a cfg, ConfigFor b cfg)
type instance ConfigFor (a :<>: b) cfg = (ConfigFor a cfg, ConfigFor b cfg)

-- Automating this is hard---TH can't enumerate exports. Add an event whenever you need it, for now.
type instance ConfigFor MessageCreateEvent cfg = ()


-- |The AradiaT transformer is the top-level monad for all bots
newtype AradiaT cfg m a = AradiaT { runAradiaT :: ReaderT cfg m a }
  deriving (Functor, Applicative, Alternative
           , Monad, MonadIO, MonadPlus
           , MonadReader cfg)


instance MonadTrans (AradiaT cfg) where
  lift = AradiaT . lift

instance MonadTransControl (AradiaT cfg) where
  type StT (AradiaT cfg) a = StT (ReaderT cfg) a
  liftWith = defaultLiftWith AradiaT runAradiaT
  restoreT = defaultRestoreT AradiaT

instance MonadBase b m => MonadBase b (AradiaT cfg m) where
  liftBase = liftBaseDefault

instance MonadBaseControl b m => MonadBaseControl b (AradiaT cfg m) where
  type StM (AradiaT cfg m) a = ComposeSt (AradiaT cfg) m a
  liftBaseWith = defaultLiftBaseWith
  restoreM  = defaultRestoreM

instance (Monad m, MonadIO m, MonadPlus m, MonadBaseControl IO m, HasAradiaConfig cfg) => DiscordAuth (AradiaT cfg m) where
  auth = Bot <$> view aradiaToken
  version = return "dev"

