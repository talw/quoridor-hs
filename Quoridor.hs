module Quoridor
where

import qualified Data.Set as S
import Control.Monad.State
import Data.List (elemIndex, findIndex, find)
import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>))
import Control.Monad (liftM2)
import Data.Functor.Identity

type Cell = (Char, Int)
type Gate = (HalfGate, HalfGate)
type HalfGate = (Cell, Cell)
type HalfGates = S.Set HalfGate
type BoardSize = Int

type Game = State GameState
{-type Bla m a = StateT GameState m a-}

data Player = Player {
  color :: Color,
  pos :: Cell,
  gatesLeft :: Int
} deriving Show

data Turn = PutGate Gate | Move Cell

data Color = Black | White
  deriving (Eq, Show)

data GameState = GameState {
  players :: [Player],
  gates :: HalfGates,
  currP :: Player
} deriving Show

isAdj :: Cell -> Cell -> Bool
isAdj (c,i) (c',i') = fromEnum c - fromEnum c' == abs 1 &&
                        i - i' == abs 1

isHalfGateSpaceClear  :: HalfGate -> HalfGates -> Bool
isHalfGateSpaceClear = (not .) . S.member

isGateSpaceClear  :: Gate -> HalfGates -> Bool
isGateSpaceClear (h1, h2) = liftM2 (&&)
  (isHalfGateSpaceClear h1) (isHalfGateSpaceClear h2)

insertGate :: Gate -> HalfGates -> HalfGates
insertGate (h1, h2) = S.insert h2 . S.insert h1

isVacant :: Cell -> [Player] -> Bool
isVacant c = all ((c /=) . pos)

playerIndex :: Color -> [Player] -> Int
playerIndex c = fromMaybe (error "playerIndex") . findIndex ((c ==) . color)

changeCurrPlayer :: Game ()
changeCurrPlayer = do
  gs <- get
  let playerList = players gs
  let newIndex = playerIndex (color $ currP gs) playerList `mod` length playerList
  modify $ \s -> s {currP = playerList !! newIndex}

changePosition :: Cell -> Game ()
changePosition c = modify $ \s -> s {currP = (currP s) {pos = c}}

isValidTurn :: Turn -> Game Bool
isValidTurn (Move c) = do
  gs <- get
  let cp = currP gs
  let adjacent = isAdj c $ pos cp
  let noGate = isHalfGateSpaceClear (c, pos cp) $ gates gs
  let vacant = isVacant c $ players gs
  return $ adjacent && noGate && vacant
isValidTurn (PutGate g) = do
  gates' <- gets gates
  let gateClear = isGateSpaceClear g gates'
  p <- gets currP
  let haveGates = gatesLeft p > 0
  return $ gateClear && haveGates

{-putGate :: Gate -> Game Bool-}
{-putGate g = do-}
  {-gates' <- gets gates-}
  {-let noGate = isGateSpaceClear g gates'-}
  {-p <- gets currP-}
  {-let haveGates = gatesLeft p > 0-}
  {-when noGate $ modify $ \s -> s {gates = insertGate g gates'}-}
  {-return noGate-}

actTurn :: Turn -> Game ()
actTurn (Move c) = modify $ \s -> s {currP = (currP s) {pos = c}}
actTurn (PutGate g) = modify $ \s -> s {
    gates = insertGate g (gates s),
    currP = (currP s) {gatesLeft = gatesLeft (currP s) - 1}
  }

{-makeMove :: Cell -> Game Bool-}
{-makeMove dest = do-}
  {-valid <- isValidMove dest-}
  {-when valid $ changePosition dest >> changeCurrPlayer-}
  {-return valid-}

makeTurn :: Turn -> Game Bool
makeTurn t = do
  valid <- isValidTurn t
  when valid $ actTurn t >> changeCurrPlayer
  return valid


isGameOver :: BoardSize -> Game (Maybe Color)
isGameOver size = do
  playerList <- gets players
  return $ color <$> find didPlayerWin playerList
 where didPlayerWin (Player c (_,row) _)
         | c == White = row == 0
         | c == Black = row == size - 1
