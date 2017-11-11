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

import Network.Discord

data AradiaConfig = AradiaConfig { _aradiaToken :: String
                                 , _aradiaPrefix :: Text }
  deriving (Show)

newtype AradiaT m a = AradiaT { runAradiaT :: ReaderT AradiaConfig m a }
  deriving (Functor, Applicative, Alternative
           , Monad, MonadIO, MonadPlus
           , MonadReader AradiaConfig)

instance MonadTrans AradiaT where
  lift = AradiaT . lift

instance MonadTransControl AradiaT where
  type StT AradiaT a = StT (ReaderT AradiaConfig) a
  liftWith = defaultLiftWith AradiaT runAradiaT
  restoreT = defaultRestoreT AradiaT

instance MonadBase b m => MonadBase b (AradiaT m) where
  liftBase = liftBaseDefault

instance MonadBaseControl b m => MonadBaseControl b (AradiaT m) where
  type StM (AradiaT m) a = ComposeSt AradiaT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM  = defaultRestoreM
  
makeLenses ''AradiaConfig

instance (Monad m, MonadIO m, MonadPlus m, MonadBaseControl IO m) => DiscordAuth (AradiaT m) where
  auth = Bot <$> view aradiaToken
  version = return "dev"

