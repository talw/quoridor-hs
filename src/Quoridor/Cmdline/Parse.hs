module Quoridor.Cmdline.Parse
  ( parseTurn,
    parseMessage
  ) where

import           Control.Applicative             (pure, (<*), (<*>), (*>))
import           Data.Char                       (toUpper)
import           Data.Functor                    ((<$>))
import           Data.List                       (elemIndex)
import           Data.Maybe                      (fromJust)

import           Text.Parsec                     (Parsec, anyChar, char, digit,
                                                  eof, many1, manyTill, oneOf,
                                                  parse, spaces, (<|>))

import           Quoridor
import           Quoridor.Cmdline.Messages       (validMovesChars)
import           Quoridor.Cmdline.Network.Common (Message (TurnMsg, ChatMsg))
import           Quoridor.Helpers

type Parse = Parsec String ()

-- helper functions

pMessage :: Color -> Parse Message
pMessage col =
  (TurnMsg <$> pTurn) <|> (ChatMsg col <$> pChat)

-- t STRING
pChat :: Parse String
pChat = do
  char 't'
  asToken $ manyTill anyChar eof

pTurn :: Parse Turn
pTurn = (pMove <|> pShortCutMove <|> pPutGate) <* eof

-- m y x
pMove :: Parse Turn
pMove = do
  char 'm'
  Move <$> pCell

-- one of 'validMovesChars', translated
-- into their index.
pShortCutMove :: Parse Turn
pShortCutMove = ShortCutMove . translate <$> oneOf validMovesChars
 where translate c = fromJust $ elemIndex c validMovesChars

-- g y x h|v
pPutGate :: Parse Turn
pPutGate = do
  char 'g'
  PutGate .: gateUpperLeft <$> pCell <*> pDirection

pCell :: Parse Cell
pCell = (,) <$> pInt <*> pInt

pDirection :: Parse Direction
pDirection = cToDirection <$> asToken (oneOf "hv")
  where cToDirection = (read :: String -> Direction) . pure . toUpper

pInt :: Parse Int
pInt = (read :: String -> Int) <$> asToken (many1 digit)

asToken :: Parse a -> Parse a
asToken p = spaces *> p

parseShowErrMsgs :: Parse a -> String -> Either String a
parseShowErrMsgs p str = func $ parse p "" str
  where func (Left errMsgs) = Left $ show errMsgs
        func (Right x) = Right x


-- exported functions

-- | Given a string representing a turn,
-- Parses it and returns the Turn.
-- This is used by the local client, which doesn't use
-- the Message type
--
-- Note: This is not the same string as show turn, it is
-- a more concise, for example: "m y x" to move
-- to (y,x)
parseTurn :: String -> Either String Turn
parseTurn = parseShowErrMsgs pTurn

-- | Aggregates the turn parser with a chat parser to
-- make a Message parser
parseMessage :: Color -> String -> Either String Message
parseMessage col = parseShowErrMsgs $ pMessage col
