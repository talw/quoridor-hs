module Quoridor.Cmdline.Parse
  ( parseTurn
  ) where

import Control.Applicative (pure)
import Data.Char           (toUpper)
import Data.Functor        ((<$>))
import Data.List           (elemIndex)
import Data.Maybe          (fromJust)

import Text.Parsec (Parsec, char, digit, eof, many1, oneOf, parse, spaces,
                    (<|>))

import Quoridor
import Quoridor.Cmdline.Messages (validMovesChars)

type Parse = Parsec String ()

-- helper functions

pTurn :: Parse Turn
pTurn = do
  res <- pMove <|> pShortCutMove <|> pPutGate
  eof
  return res

-- m y x
pMove :: Parse Turn
pMove = do
  char 'm'
  Move <$> pCell

pShortCutMove :: Parse Turn
pShortCutMove = do
    c <- oneOf validMovesChars
    return $ ShortCutMove $ translate c
  where translate c = fromJust $ elemIndex c validMovesChars

-- g y x h|v
pPutGate :: Parse Turn
pPutGate = do
  char 'g'
  c <- pCell
  PutGate . gateUpperLeft c <$> pDirection

pCell :: Parse Cell
pCell = do
  y <- pInt
  x <- pInt
  return (y,x)

pDirection :: Parse Direction
pDirection = cToDirection <$> asToken (oneOf "hv")
  where cToDirection = (read :: String -> Direction) . pure . toUpper

pInt :: Parse Int
pInt = (read :: String -> Int) <$> asToken (many1 digit)

asToken :: Parse a -> Parse a
asToken p = spaces >> p



-- exported functions

-- | Given a string representing a turn,
-- Parses it and returns the Turn
--
-- Note: This is not the same string as show turn, it is
-- a more concise, for example: "m y x" to move
-- to (y,x)
parseTurn :: String -> Either String Turn
parseTurn s = func $ parse pTurn "" s
  where func (Left errMsgs) = Left $ show errMsgs
        func (Right x) = Right x
