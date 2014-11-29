module Quoridor
where

import qualified Data.Set as S
import Data.List (elemIndex, findIndex, find)
import Data.Maybe (fromJust)
import Control.Applicative ((<$>))
import Control.Monad (liftM2, join)
import qualified Data.Map as M
import Control.Monad.State

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
  deriving (Read, Show)

data Color = Black | White
  deriving (Eq, Show, Ord)

data GameState = GameState {
  playerLoop :: [Player],
  halfGates :: HalfGates
} deriving Show



--- static data

andP :: (a -> Bool) -> (a -> Bool) -> a -> Bool
andP = liftM2 (&&)

initialGameState :: GameState
initialGameState = GameState {
                     playerLoop = concat $ repeat [initP Black, initP White],
                     halfGates = S.empty
                   }
  where initP c = Player {
                    color = c,
                    pos = lookup' c startPos,
                    gatesLeft = gatesPerPlayer
                  }


gatesPerPlayer :: Int
gatesPerPlayer = 10

boardSize :: BoardSize
boardSize = 9

startPos :: M.Map Color Cell
startPos = M.fromList [(Black, (boardSize - 1,boardSize `div` 2)),
                       (White, (0, boardSize `div` 2))]



--- helper functions

modifyCurrP :: (Player -> Player) -> GameState -> GameState
modifyCurrP f gs = gs {playerLoop = loop (playerList' gs)}
  where playerList' s = f (currP s) : tail (playerList s)
        loop = concat . repeat

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

getAdj :: Cell -> [Cell]
getAdj c@(y,x) = filter isValidCell adjs
  where adjs = [(y-1,x),(y+1,x),(y,x-1),(y,x+1)]

isValidCell :: Cell -> Bool
isValidCell = allT $ (>= 0) `andP` (< boardSize)
  where allT pred (a,b) = all pred [a,b]

isHalfGateSpaceClear  :: HalfGate -> HalfGates -> Bool
isHalfGateSpaceClear = (not .) . S.member

isGateSpaceClear  :: Gate -> HalfGates -> Bool
isGateSpaceClear (h1, h2) =
  isHalfGateSpaceClear h1 `andP` isHalfGateSpaceClear h2

gateToCells :: Gate -> [Cell]
gateToCells ((a,b),(c,d)) = [a,b,c,d]

data Direction = H | V deriving (Show, Read)
gateUpperLeft :: Cell -> Direction -> Gate
gateUpperLeft (y,x) H = (((y,x),(y+1,x)),((y,x+1),(y+1,x+1)))
gateUpperLeft (y,x) V = (((y,x),(y,x+1)),((y+1,x),(y+1,x+1)))

symHG :: HalfGate -> HalfGate
symHG (y,x) = (x,y)

insertGate :: Gate -> HalfGates -> HalfGates
insertGate (h1, h2) hgs = foldr S.insert hgs hgsToInsert
  where hgsToInsert = [h1, symHG h1, h2, symHG h2]

isVacant :: Cell -> GameState -> Bool
isVacant c = all ((c /=) . pos) . playerList

playerIndex :: Color -> [Player] -> Int
playerIndex c = fromJust . findIndex ((c ==) . color)

isWinningCell :: Player -> Cell -> Bool
isWinningCell p (cy,_) = cy + startY == boardSize - 1
  where (startY,_) = lookup' (color p) startPos

dfs :: Cell -> (Cell -> Bool) -> GameState -> Bool
dfs from pred gs = go from $ S.insert from S.empty
  where
    go from visited
      | pred from = True
      | otherwise = any throughThis reachableCells
      where
        reachableCells = filter (noGatePred `andP` vacantPred) $ getAdj from
          where noGatePred adj = not $ S.member (from,adj) $ halfGates gs
                vacantPred adj = isVacant adj gs
        throughThis c
          | S.member c visited = False
          | otherwise = go c $ S.insert c visited



--- Game functions

changeCurrPlayer :: Game ()
changeCurrPlayer = modify $ \s -> s {playerLoop = tail $ playerLoop s}

isValidTurn :: Turn -> Game Bool
isValidTurn (Move c@(cY,cX)) = do
  gs <- get
  let cpp@(cppX, cppY) = pos $ currP gs
      isHGClear = flip isHalfGateSpaceClear $ halfGates gs
      isStraight = cppY == cY || cppX == cX
      isValidJump
        | isStraight =
            let midC = ((cY + cppY) `div` 2, (cX + cppX) `div` 2)
                midNotVacant = not $ isVacant midC gs
                noGate = isHGClear (c, midC)
                noGate2 = isHGClear (midC, cpp)
            in  midNotVacant && noGate && noGate2
        | otherwise =
            let isSideHop (y,x) =
                  let midNotVacant = not $ isVacant (y,x) gs
                      gateExist = not $ isHGClear
                        ( (y + (y - cppY), x + (x - cppX)),
                          (y,x) )
                      noGate = isHGClear ((y,x), c)
                  in midNotVacant && noGate && gateExist
            in any isSideHop [(cY, cppX),(cppY, cX)]

      vacant = isVacant c gs
      validCell = isValidCell c
      validMove = case distance c cpp of
        1 -> isHGClear (c, cpp)
        2 -> isValidJump
        _ -> False
  return $ validCell && validMove && vacant

isValidTurn (PutGate g) = do
  gs <- get
  let validGate = all isValidCell $ gateToCells g
      hgs = halfGates gs
      p = currP gs
      noOtherGate = isGateSpaceClear g hgs
      haveGates = gatesLeft p > 0
      wontBlockPlayer p = dfs (pos p) (isWinningCell p) $
        gs { halfGates = insertGate g hgs }
      wontBlock = all wontBlockPlayer $ playerList gs
  return $ validGate && noOtherGate && haveGates && wontBlock

actTurn :: Turn -> Game ()
actTurn (Move c) = modify $ modifyCurrP $ \p -> p {pos = c}

actTurn (PutGate g) = do
    modify $ \s -> s { halfGates = insertGate g (halfGates s) }
    modify $ modifyCurrP $ \p -> p {gatesLeft = gatesLeft p - 1}



--- exported functions

makeTurn :: Turn -> Game Bool
makeTurn t = do
  valid <- isValidTurn t
  when valid $ actTurn t >> changeCurrPlayer
  return valid

getWinner :: Game (Maybe Color)
getWinner = do
  playerList <- gets playerList
  return $ color <$> find (\p -> isWinningCell p (pos p)) playerList
