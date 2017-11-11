module Aradia.Command where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class

import Data.Char
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void

import Network.Discord

import Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

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
        return $ T.stripStart <$> T.stripPrefix (prefix `T.append` commandName p) c

respond :: DiscordAuth m => Snowflake -> Text ->  DiscordApp m ()
respond chan text = void . doFetch $ CreateMessage chan text Nothing


-- parser util stuff
type Parser = Parsec Void Text

isHorizontalSpace :: Char -> Bool
isHorizontalSpace = (== Space) . generalCategory

spaceConsumer :: Parser ()
spaceConsumer = L.space C.space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

stringLexeme :: Parser Text
stringLexeme = lexeme stringLiteral <?> "string"

stringLiteral :: Parser Text
stringLiteral = (C.char '"' >> T.pack <$> manyTill C.anyChar (C.char '"'))
                <|> T.pack <$> (some (C.satisfy (not . isSpace) <?> "character"))

