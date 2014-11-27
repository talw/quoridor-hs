module Quoridor
where

import qualified Data.Set as S
import Control.Monad.State
import Data.List (elemIndex, findIndex, find)
import Data.Maybe (fromJust)
import Control.Applicative ((<$>))
import Control.Monad (liftM2, join)
import qualified Data.Map as M

type Cell = (Int, Int) -- y, x
type Gate = (HalfGate, HalfGate)
type HalfGate = (Cell, Cell)
type HalfGates = S.Set HalfGate
type BoardSize = Int

type Game = State GameState

data Player = Player {
  color :: Color,
  pos :: Cell,
  gatesLeft :: Int
} deriving (Show, Eq)

data Turn = PutGate Gate | Move Cell

data Color = Black | White
  deriving (Eq, Show, Ord)

data GameState = GameState {
  playerLoop :: [Player],
  gates :: HalfGates
} deriving Show



--- static data

boardSize :: BoardSize
boardSize = 9

startPos :: M.Map Color Cell
startPos = M.fromList [(Black, (boardSize - 1,boardSize `div` 2)),
                       (White, (0, boardSize `div` 2))]



--- helper functions

modifyCurrP :: (Player -> Player) -> GameState -> GameState
modifyCurrP f gs = gs {playerLoop = loop (playerList' gs)}
  where playerList' s = f (currP s) : tail (playerList s)
        loop list = list ++ loop list

playerList :: GameState -> [Player]
playerList gs = head pl : takeWhile (head pl /=) (tail pl)
  where pl = playerLoop gs

currP :: GameState -> Player
currP = head . playerLoop

lookup' :: Ord k => k -> M.Map k a -> a
lookup' = (fromJust .) . M.lookup

distance :: Cell -> Cell -> Int
distance (y,x) (y',x') = abs (y' - y) + abs (x' - x)

isAdj :: Cell -> Cell -> Bool
isAdj = ((1 ==) .) . distance

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
playerIndex c = fromJust . findIndex ((c ==) . color)



--- Game functions

changeCurrPlayer :: Game ()
changeCurrPlayer = modify $ \s -> s {playerLoop = tail $ playerLoop s}

isValidTurn :: Turn -> Game Bool
isValidTurn (Move c@(cY,cX)) = do
  gs <- get
  let cpp@(cppX, cppY) = pos $ currP gs
      vacant = isVacant c $ playerLoop gs
      isHGClear = flip isHalfGateSpaceClear $ gates gs
      valid = case distance c cpp of
        1 -> isHGClear (c, cpp)
        2
          | isStraight c cpp -> let midC = ((cY + cppY) `div` 2, (cX + cppX) `div` 2)
                                    midNotVacant = not $ isVacant midC $ playerLoop gs
                                    noGate = isHGClear (c, midC)
                                    noGate2 = isHGClear (midC, cpp)
                                in  midNotVacant && noGate && noGate2
          | otherwise -> let isSideHop (y,x) =
                               let midNotVacant = not $ isVacant (y,x) $ playerLoop gs
                                   gateExist = not $ isHGClear
                                     ( (y + (y - cppY), x + (x - cppX)),
                                       (y,x) )
                                   noGate = isHGClear ((y,x), cpp)
                               in midNotVacant && noGate && gateExist
                         in any isSideHop [(cY, cppX),(cppY, cX)]
        _ -> False
  return $ valid && vacant
 where isStraight (y,x) (y',x') = y == y' || x == x'

isValidTurn (PutGate g) = do
  gates' <- gets gates
  p <- gets currP
  let noGate = isGateSpaceClear g gates'
      haveGates = gatesLeft p > 0
  return $ noGate && haveGates

actTurn :: Turn -> Game ()
actTurn (Move c) = modify $ modifyCurrP $ \p -> p {pos = c}

actTurn (PutGate g) = do
    modify $ \s -> s { gates = insertGate g (gates s) }
    modify $ modifyCurrP $ \p -> p {gatesLeft = gatesLeft p - 1}



--- exported functions

makeTurn :: Turn -> Game Bool
makeTurn t = do
  valid <- isValidTurn t
  when valid $ actTurn t >> changeCurrPlayer
  return valid

isGameOver :: Game (Maybe Color)
isGameOver = do
  playerList <- gets playerLoop
  return $ color <$> find didPlayerWin playerList
 where didPlayerWin (Player c (currY,_) _) = let (startY,_) = lookup' c startPos
                                            in currY + startY == boardSize - 1
