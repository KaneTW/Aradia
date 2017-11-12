module Aradia.Command.Games where

import Control.Monad

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Format

import Network.Discord

import Text.Megaparsec
import qualified Text.Megaparsec.Char as C

import System.Random

import Aradia.Command
import Aradia.Types
import Aradia.Utils

data Relationship = Flushed Text Text
                  | Pale Text Text
                  | Calignious Text Text
                  | Ashen Text Text Text
  deriving (Show, Eq, Ord)

parseRelationship :: Parser Relationship
parseRelationship = choice [try pale, try calignious, try ashen, flushed]
  where
    flushed = Flushed <$> stringLexeme <* optional heart <*> hidden stringLexeme
    pale = Pale <$> stringLexeme <* diamond <*> stringLexeme
    calignious = Calignious <$> stringLexeme <* spade <*> stringLexeme
    ashen = Ashen <$> stringLexeme <* club <*> stringLexeme <* club <*> stringLexeme

    heart = choice [ hidden $ symbol ":heart:"
                   , hidden $ symbol ":hearts:"
                   , hidden $ symbol "❤"
                   , symbol "♥"
                   , symbol "<3" <* C.spaceChar ] -- parsing ambiguity with "<3<"
    diamond = choice [ hidden $ symbol ":diamonds:"
                     , symbol "♦"
                     , symbol "<>" ]
    spade = choice [ hidden $ symbol ":spades:"
                   , symbol "♠"
                   , symbol "<3<" ]
    club = choice [ hidden $ symbol ":clubs:"
                  , symbol "♣"
                  , symbol "o8<" ]

-- probably could be done cleaner, who cares
printRelationship :: Relationship -> Text
printRelationship (Flushed l r) = T.unwords [l, "♥", r]
printRelationship (Pale l r) = T.unwords [l, "♦", r]
printRelationship (Calignious l r) = T.unwords [l, "♠", r]
printRelationship (Ashen l m r) = T.unwords [l, "♣", m, "♣", r]

data Ship

type instance ConfigFor Ship cfg = ()

instance AradiaCommand Ship where
  commandName _ = "ship"
  commandUsage _ = "<a> ♥/♦/♣ <b>, <a> ♣ <b> ♣ <c>"
  commandDescription _ = "checks the shipping wall"
  handleMessage p Message{messageChannel = chan} text = respond chan response
    where
      response = case parse parseRelationship "input" text  of
        Left e -> format' "{}\nUsage: {}" (parseErrorTextPretty e, commandUsage p)
        Right r -> format' "{}\n`{}` ==> {}%" (printRelationship r
                                            , asBars 10 $ compat r
                                            , fixed 0 (100 * compat r))
      compat r = fst . random $ rng r :: Double
      rng r = pureMTFromText $ printRelationship r
        
