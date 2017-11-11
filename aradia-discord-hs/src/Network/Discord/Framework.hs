-- | Provides a convenience framework for writing Discord bots without dealing with Pipes
{-# LANGUAGE TypeOperators, RankNTypes, TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Network.Discord.Framework where
  import Data.Proxy
  import Control.Applicative
  import Control.Concurrent.Lifted hiding (fork)
  import qualified Control.Concurrent.Lifted as L
  import Control.Lens
  import Control.Monad.Base
  import Control.Monad.Trans.Class
  import Control.Monad.Trans.Control
  import System.IO.Unsafe (unsafePerformIO)

  import Network.Discord.Rest
  import Network.Discord.Gateway
  import Network.Discord.Types

  import Control.Monad.Reader
  import Data.Hashable
  import Network.WebSockets (Connection)

  
  newtype DiscordApp m a = DiscordApp 
    { runEvent :: ReaderT (Connection, Event) m a }
    deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadIO, MonadReader (Connection, Event))

  instance MonadTrans DiscordApp where
    lift = DiscordApp . lift

  instance MonadTransControl DiscordApp where
    type StT DiscordApp a = StT (ReaderT (Connection, Event)) a
    liftWith = defaultLiftWith DiscordApp runEvent
    restoreT = defaultRestoreT DiscordApp

  instance MonadBase b m => MonadBase b (DiscordApp m) where
    liftBase = liftBaseDefault

  instance MonadBaseControl b m => MonadBaseControl b (DiscordApp m) where
    type StM (DiscordApp m) a = ComposeSt DiscordApp m a
    liftBaseWith = defaultLiftBaseWith
    restoreM  = defaultRestoreM

  instance DiscordAuth m => DiscordAuth (DiscordApp m) where
    auth    = lift auth
    version = lift version

  rateLimits :: Vault (DiscordApp m) [(Int, Int)]
  rateLimits = unsafePerformIO $ newMVar []
  {-# NOINLINE rateLimits #-}

  delete :: Eq a => [(a, b)] -> a -> [(a, b)]
  delete ((a, b):xs) a'
    | a == a'   = delete xs a'
    | otherwise = (a, b):delete xs a'
  delete [] _ = []

  modify :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
  modify a' b' ((a, b):xs)
    | a == a'   = (a', b'): delete xs a
    | otherwise = (a, b): modify a' b' xs
  modify a' b' [] = [(a', b')]

  instance DiscordAuth m => DiscordRest (DiscordApp m) where
    getRateLimit f = lookup' (hash f) =<< get rateLimits
      where
        lookup' :: (Eq a, Monad m) => a -> [(a, b)] -> m (Maybe b)
        lookup' a' ((a, b):xs)
          | a' == a   = return (Just b)
          | otherwise = lookup' a' xs
        lookup' _ [] = return Nothing
    setRateLimit f l = put rateLimits =<< modify (hash f) l `fmap` get rateLimits

  instance DiscordAuth m => DiscordGate m (DiscordApp m)  where
    type Vault (DiscordApp m) = L.MVar

    data VaultKey (DiscordApp m) a = Store (L.MVar a)
    get = liftIO . readMVar
    put s v = liftIO $ do
      _ <- tryTakeMVar s
      putMVar s v

    sequenceKey = Store $ unsafePerformIO newEmptyMVar
    {-# NOINLINE sequenceKey #-}
    storeFor (Store var) = return var

    connection = view _1
    feed m event = do
      liftIO $ print "Running event handler"
      c <- connection
      _ <- lift $ runReaderT (runEvent m) (c, event)
      liftIO $ print "Returning from handler"

    run m conn = runReaderT (runEvent $ eventStream Create m) (conn, Nil)
    fork m = do
      c <- connection
      _ <- view _2 >>= lift . L.fork . curry (runReaderT (runEvent m)) c
      return ()

  class DiscordRest m => EventMap f m where
    type Domain f
    type Codomain f
    mapEvent :: Proxy f -> Domain f -> m (Codomain f)

  data a :> b
  data a :<>: b

  instance (DiscordRest m, EventMap f m, EventMap g m, Codomain f ~ Domain g)
     => EventMap (f :> g) m where

     type Domain   (f :> g) = Domain f
     type Codomain (f :> g) = Codomain g

     mapEvent p event = mapEvent b =<< mapEvent a event
      where
        (a, b) = split p
        split :: Proxy (a :> b) -> (Proxy a, Proxy b)
        split _ = (Proxy, Proxy)

  instance (DiscordRest m, EventMap f m, EventMap g m
    , Domain f ~ Domain g, Codomain f ~ Codomain g)
    => EventMap (f :<>: g) m where
    
    type Domain   (f :<>: g) = Domain f
    type Codomain (f :<>: g) = Codomain f

    mapEvent p event = mapEvent a event <|> mapEvent b event
      where
        (a, b) = split p
        split :: Proxy (a :<>: b) -> (Proxy a, Proxy b)
        split _ = (Proxy, Proxy)

