module Aradia.Command.Help where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class

import Data.Void
import Data.Monoid
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Network.Discord

import Aradia.Command
import Aradia.Types

data Help cmds

class MkHelp cmds where
  mkHelp :: HasAradiaConfig cfg => Proxy cmds -> AradiaT cfg IO Text

instance MkHelp b => MkHelp (a :> b) where
  mkHelp _ = mkHelp (Proxy :: Proxy b)

instance (MkHelp a, MkHelp b) => MkHelp (a :<>: b) where
  mkHelp _ = liftM2 (\l r -> l <> "\n\n" <> r) (mkHelp (Proxy :: Proxy a)) (mkHelp (Proxy :: Proxy b))

instance AradiaCommand a => MkHelp (Command a) where
  mkHelp p = do
    prefix <- view aradiaPrefix
    return $ commandName p <> " -- " <> commandDescription p
      <> "\nusage: " <> prefix <> commandName p <> " " <> commandUsage p

instance MkHelp Void where
  mkHelp _ = return ""

type instance ConfigFor (Help cmds) cfg = ()

instance MkHelp cmds => AradiaCommand (Help cmds) where
  commandName _ = "help"
  commandUsage _ = ""
  commandDescription _ = "prints a help message"

  handleMessage _ Message{messageChannel = chan} _
    = lift (mkHelp (Proxy :: Proxy (cmds :<>: Command (Help Void)))) >>= respond chan
