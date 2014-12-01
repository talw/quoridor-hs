module Quoridor.Cmdline.Parse (parseTurn)
where

import Quoridor
import Text.Parsec
import Data.Char (toUpper)
import Data.Functor ((<$>))
import Control.Applicative (pure)
import Data.Either (isLeft)

type Parse = Parsec String ()

-- helper functions

pTurn :: Parse Turn
pTurn = do
  res <- pMove <|> pPutGate
  eof
  return res

-- m y x
pMove :: Parse Turn
pMove = do
  char 'm'
  Move <$> pCell

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

parseTurn :: String -> Either String Turn
parseTurn s = func $ parse pTurn "" s
  where func (Left errMsgs) = Left $ show errMsgs
        func (Right x) = Right x
