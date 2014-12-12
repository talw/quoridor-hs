{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Quoridor

where

import Quoridor.Helpers
import qualified Data.Set as S
import Data.List (elemIndex, findIndex, find)
import Data.Maybe (fromJust)
import Control.Applicative ((<$>), Applicative)
import Control.Monad (liftM2, join)
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Catch

type Cell = (Int, Int) -- y, x
type Gate = (HalfGate, HalfGate)
type HalfGate = (Cell, Cell)
type HalfGates = S.Set HalfGate
type BoardSize = Int

{-type Game = State GameState-}
newtype Game m a = Game (ReaderT GameConfig (StateT GameState m) a)
  deriving (Monad, MonadState GameState, MonadIO
           ,Applicative, Functor, MonadReader GameConfig
           ,MonadThrow, MonadCatch, MonadMask)

instance MonadTrans Game where
  lift = Game . lift . lift

runGame :: Functor m => Game m a -> GameConfig -> m ()
runGame g gc = void $ runGameWithGameState g (initialGameState gc) gc

runGameWithGameState :: Game m a -> GameState -> GameConfig -> m (a, GameState)
runGameWithGameState (Game g) gs gc = runStateT (runReaderT g gc) gs

data Player = Player {
  color :: Color,
  pos :: Cell,
  gatesLeft :: Int
} deriving (Show, Eq, Read)

data Turn = PutGate Gate | Move Cell
  deriving (Read, Show)

data Color = Black | White | Red | Green
  deriving (Eq, Show, Ord, Enum, Read)

data GameState = GameState {
  playerList :: [Player],
  halfGates :: HalfGates,
  winner :: Maybe Color
} deriving (Show, Read)

data GameConfig = GameConfig {
  gatesPerPlayer :: Int,
  boardSize :: Int,
  numOfPlayers :: Int
} deriving (Show, Read)

--- static data

initialGameState :: GameConfig -> GameState
initialGameState gc =
  GameState {
    playerList = take (numOfPlayers gc) $ map (initP . toEnum) [0..],
    halfGates = S.empty,
    winner = Nothing
  }
  where initP c = Player {
                    color = c,
                    pos = unsafeLookup c $ startPos $ boardSize gc,
                    gatesLeft = gatesPerPlayer gc
                  }

defaultGameConfig :: GameConfig
defaultGameConfig = GameConfig {
  gatesPerPlayer = 10,
  boardSize = 9,
  numOfPlayers = 2
}

startPos :: Int -> M.Map Color Cell
startPos bs = M.fromList [(Black, (bs - 1,bs `div` 2))
                         ,(White, (0, bs `div` 2))
                         ,(Red, (bs `div` 2, 0))
                         ,(Green, (bs `div` 2, bs - 1))
                         ]



--- helper functions

modifyCurrP :: (Player -> Player) -> GameState -> GameState
modifyCurrP f gs = gs {playerList = playerList'}
  where playerList' = f (currP gs) : tail (playerList gs)

currP :: GameState -> Player
currP = head . playerList

distance :: Cell -> Cell -> Int
distance (y,x) (y',x') = abs (y' - y) + abs (x' - x)

isAdj :: Cell -> Cell -> Bool
isAdj = ((1 ==) .) . distance

getAdj :: Int -> Cell -> [Cell]
getAdj bs c@(y,x) = filter (isValidCell bs) adjs
  where adjs = [(y-1,x),(y+1,x),(y,x-1),(y,x+1)]

isValidCell :: Int -> Cell -> Bool
isValidCell bs = allT $ (>= 0) `andP` (< bs)
  where allT pred (a,b) = all pred [a,b]

align :: HalfGate -> HalfGate
align (c1,c2) = (min c1 c2, max c1 c2)

isHalfGateSpaceClear  :: HalfGate -> HalfGates -> Bool
isHalfGateSpaceClear = (not .) . S.member . align

isGateSpaceClear  :: Gate -> HalfGates -> Bool
isGateSpaceClear (h1, h2) =
  isHalfGateSpaceClear h1 `andP` isHalfGateSpaceClear h2

gateToCells :: Gate -> [Cell]
gateToCells ((a,b),(c,d)) = [a,b,c,d]

data Direction = H | V deriving (Show, Read)
gateUpperLeft :: Cell -> Direction -> Gate
gateUpperLeft (y,x) H = (((y,x),(y+1,x)),((y,x+1),(y+1,x+1)))
gateUpperLeft (y,x) V = (((y,x),(y,x+1)),((y+1,x),(y+1,x+1)))

insertGate :: Gate -> HalfGates -> HalfGates
insertGate (h1, h2) = S.insert (align h2) . S.insert (align h1)

isVacant :: Cell -> GameState -> Bool
isVacant c = all ((c /=) . pos) . playerList

playerIndex :: Color -> [Player] -> Int
playerIndex c = fromJust . findIndex ((c ==) . color)

isWinningCell :: Int -> Player -> Cell -> Bool
isWinningCell bs p (cy,cx)
  | startX == bs `div` 2 = cy + startY == bs - 1
  | startY == bs `div` 2 = cx + startX == bs - 1
  where (startY,startX) = unsafeLookup (color p) (startPos bs)

getValidMoves :: Cell -> Int -> GameState -> [Cell]
getValidMoves c@(y,x) bs gs = validatedResult
  where adjs = getAdj bs c
        hgs = halfGates gs
        noHgs src = filter (\c' -> isHalfGateSpaceClear (src,c') hgs)
        result = concatMap
          (\c' -> if isVacant c' gs then [c'] else plTr c') $ noHgs c adjs
        validatedResult = filter (flip isVacant gs `andP` isValidCell bs) result

        plTr c'@(y',x') = if null $ noHgs c' [c'']
                    then noHgs c' sideCells
                    else [c'']
          where c'' = (y' + (y'-y), x' + (x'-x))
                sideCells
                  | y' == y = [(y'-1,x'),(y'+1,x')]
                  | x' == x = [(y',x'-1),(y',x'+1)]

dfs :: Cell -> (Cell -> Bool) -> Int -> GameState -> Bool
dfs from pred bs gs = evalState (go from) $ S.insert from S.empty
  where
    go from
      | pred from = return True
      | otherwise = or <$> mapM throughThis reachableCells
      where
        reachableCells = getValidMoves from bs gs
        throughThis c = do
          visited <- get
          if S.member c visited
            then return False
            else put (S.insert c visited) >> go c



--- Game functions

changeCurrPlayer :: Monad m => Game m ()
changeCurrPlayer = modify $ \s -> s {playerList = rotateList $ playerList s}

isValidTurn :: Monad m => Turn -> Game m Bool
isValidTurn (Move c) = do
  gs <- get
  bs <- reader boardSize
  return $ c `elem` getValidMoves (pos $ currP gs) bs gs

isValidTurn (PutGate g) = do
  gs <- get
  bs <- reader boardSize
  let validGate = all (isValidCell bs) $ gateToCells g
      hgs = halfGates gs
      p = currP gs
      noOtherGate = isGateSpaceClear g hgs
      haveGates = gatesLeft p > 0
      wontBlockPlayer p = dfs (pos p) (isWinningCell bs p) bs $
        gs { halfGates = insertGate g hgs }
      wontBlock = all wontBlockPlayer $ playerList gs
  return $ validGate && noOtherGate && haveGates && wontBlock

actTurn :: Monad m => Turn -> Game m ()
actTurn (Move c) = modify $ modifyCurrP $ \p -> p {pos = c}
actTurn (PutGate g) = do
    modify $ \s -> s { halfGates = insertGate g (halfGates s) }
    modify $ modifyCurrP $ \p -> p {gatesLeft = gatesLeft p - 1}



--- exported functions (for modules other than Tests)

makeTurn :: Monad m => Turn -> Game m Bool
makeTurn t = do
  valid <- isValidTurn t
  when valid $ do
    actTurn t
    checkAndSetWinner
    changeCurrPlayer
  return valid

checkAndSetWinner :: Monad m => Game m (Maybe Color)
checkAndSetWinner = do
  playerList <- gets playerList
  bs <- reader boardSize
  let mWinner = color <$> find (\p -> isWinningCell bs p (pos p)) playerList
  modify $ \s -> s { winner = mWinner }
  return mWinner
