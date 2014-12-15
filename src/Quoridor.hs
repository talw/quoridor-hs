{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Quoridor
where

import           Control.Applicative  (Applicative, (<$>))
import           Control.Monad.Catch  (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Reader (MonadReader, ReaderT, reader,
                                       runReaderT)
import           Control.Monad.State  (MonadIO, MonadState, MonadTrans, StateT,
                                       evalState, get, gets, lift, modify, put,
                                       runStateT, void, when)
import           Data.List            (find, findIndex, sort)
import qualified Data.Map             as M
import           Data.Maybe           (fromJust)
import qualified Data.Set             as S

import Quoridor.Helpers (andP, rotateList, unsafeLookup)

type Cell = (Int, Int) -- y, x

-- \ A half gate, is a game gate broken into two.
-- So that it blocks one path between two 'Cell's in the game,
-- and not two paths.
type HalfGate = (Cell, Cell)
type Gate = (HalfGate, HalfGate)
type HalfGates = S.Set HalfGate

-- | Size of the board in one dimension.
-- The board is a square
type BoardSize = Int

newtype Game m a = Game (ReaderT GameConfig (StateT GameState m) a)
  deriving ( Monad, MonadState GameState, MonadIO
           , Applicative, Functor, MonadReader GameConfig
           , MonadThrow, MonadCatch, MonadMask )

instance MonadTrans Game where
  lift = Game . lift . lift

runGame :: Functor m => Game m a -> GameConfig -> m ()
runGame g gc = void $ runGameWithGameState g (initialGameState gc) gc

runGameWithGameState :: Game m a -> GameState -> GameConfig -> m (a, GameState)
runGameWithGameState (Game g) gs gc = runStateT (runReaderT g gc) gs

data Player = Player
  { color     :: Color
  , pos       :: Cell
  , gatesLeft :: Int
  } deriving (Show, Eq, Read)

-- | Represents a turn,
-- can be either a 'Gate' put
-- or a 'Player' move
data Turn = PutGate Gate
          | Move Cell
          | ShortCutMove Int
  deriving (Read, Show)

-- | Colors to distinguish between 'Player's
data Color = Black | White | Red | Green
  deriving (Eq, Show, Ord, Enum, Read)

-- | The orientation (perhaps a better name?)
-- of the 'Gate', it can be either vertical or horizontal
data Direction = H | V
  deriving (Show, Read)

-- | Represents the game state.
-- With a list of 'Player's (the head is the current player),
-- maybe a winner, and a map of the 'Gate's, which actually
-- breaks them into 'Halfgate's.
data GameState = GameState
  { playerList :: [Player]
  , halfGates  :: HalfGates
  , winner     :: Maybe Color
  } deriving (Show, Read)

data GameConfig = GameConfig
  { gatesPerPlayer :: Int
  , boardSize      :: Int
  , numOfPlayers   :: Int
  } deriving (Show, Read)

--- static data

initialGameState :: GameConfig -> GameState
initialGameState gc =
    GameState
      { playerList = take (numOfPlayers gc) $ map (initP . toEnum) [0..]
      , halfGates  = S.empty
      , winner     = Nothing
      }
  where initP c = Player
                    { color     = c
                    , pos       = unsafeLookup c $ startPos $ boardSize gc
                    , gatesLeft = gatesPerPlayer gc
                    }

defaultGameConfig :: GameConfig
defaultGameConfig = GameConfig
  { gatesPerPlayer = 10
  , boardSize = 9
  , numOfPlayers = 2
  }

startPos :: Int -> M.Map Color Cell
startPos bs = M.fromList [ (Black, (bs - 1,bs `div` 2))
                         , (White, (0, bs `div` 2))
                         , (Red, (bs `div` 2, 0))
                         , (Green, (bs `div` 2, bs - 1))
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
getAdj bs (y,x) = filter (isValidCell bs) adjs
  where adjs = [(y-1,x),(y+1,x),(y,x-1),(y,x+1)]

isValidCell :: Int -> Cell -> Bool
isValidCell bs = allT $ (>= 0) `andP` (< bs)
  where allT predicate (a,b) = all predicate [a,b]

align :: HalfGate -> HalfGate
align (c1,c2) = (min c1 c2, max c1 c2)

isHalfGateSpaceClear  :: HalfGate -> HalfGates -> Bool
isHalfGateSpaceClear = (not .) . S.member . align

isGateSpaceClear  :: Gate -> HalfGates -> Bool
isGateSpaceClear (h1, h2) =
  isHalfGateSpaceClear h1 `andP` isHalfGateSpaceClear h2

gateToCells :: Gate -> [Cell]
gateToCells ((a,b),(c,d)) = [a,b,c,d]

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
    | otherwise = error "startPos is not properly defined."
  where (startY,startX) = unsafeLookup (color p) (startPos bs)

-- | Basically, translates a 'ShortCutMove' into the 'Move'
-- that it is a shortcut of, using the integral index that
-- is the index of the shortcut character in the list of
-- 'validMovesChars'
coerceTurn :: (Monad m, Functor m) => Turn -> Game m Turn
coerceTurn (ShortCutMove i) = do
  vmSorted <- sort <$> getCurrentValidMoves
  return $ Move $ vmSorted !! i
coerceTurn t = return t

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
                  | otherwise = error "A bug in getAdj"

dfs :: Cell -> (Cell -> Bool) -> Int -> GameState -> Bool
dfs from predicate bs gs = evalState (go from) $ S.insert from S.empty
  where
    go from'
        | predicate from' = return True
        | otherwise = or <$> mapM throughThis reachableCells
      where
        reachableCells = getValidMoves from' bs gs
        throughThis c = do
          visited <- get
          if S.member c visited
            then return False
            else put (S.insert c visited) >> go c



--- Game functions

-- | Rotates the 'Player' list to change the current player.
-- The player at the had of the player list is the current player.
changeCurrPlayer :: Monad m => Game m ()
changeCurrPlayer = modify $ \s -> s {playerList = rotateList $ playerList s}

-- | Checks if a given 'Turn' is valid, rule-wise.
-- It does it by perusing 'getCurrentValidMoves's returned
-- list of all possible valid moves.
isValidTurn :: (Monad m, Functor m) => Turn -> Game m Bool
isValidTurn (Move c) = (c `elem`) <$> getCurrentValidMoves

isValidTurn (PutGate g) = do
  gs <- get
  bs <- reader boardSize
  let validGate = all (isValidCell bs) $ gateToCells g
      hgs = halfGates gs
      cp = currP gs
      noOtherGate = isGateSpaceClear g hgs
      haveGates = gatesLeft cp > 0
      wontBlockPlayer p = dfs (pos p) (isWinningCell bs p) bs $
        gs { halfGates = insertGate g hgs }
      wontBlock = all wontBlockPlayer $ playerList gs
  return $ validGate && noOtherGate && haveGates && wontBlock

isValidTurn _ = error "bug with coerceTurn"

-- | Acts upon a single 'Turn'.
-- The difference with 'MakeTurn', is that MakeTurn calls this
-- function and does more, like changing currentPlayer and
-- checking for a winner.
actTurn :: Monad m => Turn -> Game m ()
actTurn (Move c) = modify $ modifyCurrP $ \p -> p { pos = c }
actTurn (PutGate g) = do
    modify $ \s -> s { halfGates = insertGate g (halfGates s) }
    modify $ modifyCurrP $ \p -> p { gatesLeft = gatesLeft p - 1 }
actTurn _ = error "Bug with coerceTurn"

-- | Checks if there's a winner, returning it if there is
-- and sets the winner in the 'GameState'.
checkAndSetWinner :: Monad m => Game m (Maybe Color)
checkAndSetWinner = do
  pl <- gets playerList
  bs <- reader boardSize
  let mWinner = color <$> find (\p -> isWinningCell bs p (pos p)) pl
  modify $ \s -> s { winner = mWinner }
  return mWinner



--- exported functions (for modules other than Tests)

-- | Makes a single 'Turn' in a game.
-- Changes the state ('GameState') accordingly and returns
-- whether or not a valid turn was requested.
-- If an invalid turn was requested, it can be safely assumed
-- that the GameState did not change.
makeTurn :: (Monad m, Functor m) => Turn -> Game m Bool
makeTurn t = do
  t' <- coerceTurn t
  valid <- isValidTurn t'
  when valid $ do
    actTurn t'
    checkAndSetWinner
    changeCurrPlayer
  return valid

getCurrentValidMoves :: Monad m => Game m [Cell]
getCurrentValidMoves = do
  bs <- reader boardSize
  gs <- get
  let cell = pos $ currP gs
  return $ getValidMoves cell bs gs
