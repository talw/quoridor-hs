module Quoridor.Cmdline.Render
  ( runRender
  , runRenderColor
  , putColoredStr
  ) where

import           Control.Monad.Reader (ReaderT, reader, runReaderT)
import           Control.Monad.State  (StateT, get, gets, modify, runStateT)
import           Control.Monad.Writer (Writer, runWriter, tell, void)
import           Data.List            (partition, sort, sortBy)
import qualified Data.Set             as S (toAscList)

import qualified Data.DList          as D
import qualified System.Console.ANSI as CA

import Quoridor
import Quoridor.Cmdline.Messages (msgInputInstr, validMovesChars)



type Render = ReaderT GameConfig
                (StateT RenderState
                (Writer (D.DList Char)))

data RenderState = RenderState
  { players             :: [Player]
  , vertHalfGates       :: [HalfGate]
  , horizHalfGates      :: [HalfGate]
  , validMoves          :: [Cell]
  , leftValidMovesChars :: String
  }


--- exported functions

-- | Returns a String of the game board along with some basic info
runRender :: GameState -> GameConfig -> [Cell] -> String
runRender gs gc vms = D.toList w
  where (_,w) =
          runWriter (runStateT (runReaderT (render cp) gc) initialRenderState)
        initialRenderState = RenderState
          psSorted vhgs hhgs vmSorted validMovesChars
        psSorted = sortPlayers $ playerList gs
        vmSorted = sort vms
        (hhgs, vhgs) = partitionHalfGates $ S.toAscList $ halfGates gs
        cp = currP gs

-- | Returns a String of the game board along with some basic info,
-- and a series of IO () actions, one per character, which describe how
-- to set the terminal color. putColoredStr can be used to apply
-- those actions automatically
runRenderColor :: GameState -> GameConfig -> [Cell] -> (String, [IO ()])
runRenderColor = ((addColor .) .) . runRender

-- | Given an input such as the output of runRenderColor, writes the
-- game board along with some basic info, to the screen, applying
-- the IO actions to colorize the output.
putColoredStr :: (String, [IO ()]) -> IO ()
putColoredStr (str, actions) = mapM_ putColoredChar $ zip str actions
  where putColoredChar (c, action) = action >> putChar c


--- helper functions

render ::  Player -> Render ()
render cp = do
  renderBoard
  tellLine msgInputInstr
  tellLine $ "It's " ++ show (color cp) ++ "'s Turn."
        ++ " " ++ show (gatesLeft cp) ++ " gates left."
  tellNewLine

tellStr :: String -> Render ()
tellStr str = tell $ D.fromList str

tellLine :: String -> Render ()
tellLine str = tellStr str >> tellNewLine

tellNewLine :: Render ()
tellNewLine = tellStr "\n"

renderBoard :: Render ()
renderBoard = do
  bs <- reader boardSize
  let go y
        | y == bs = return ()
        | otherwise = do
            let lineRuler = show y ++ tail linePadding
            tellStr lineRuler >> renderTileRow y
            tellStr linePadding >> renderBetweenRow y
            go $ y+1
      tellRulerLine = tellLine $
        linePadding ++ unwords (map show [0..bs-1])
  tellRulerLine
  tellNewLine
  go 0
  tellRulerLine
  tellNewLine

renderTileRow :: Int -> Render ()
renderTileRow row = do
  bs <- reader boardSize
  let go y x
        | x == bs = void $ tellStr "\n"
        | otherwise = do
            RenderState ps vhgs _ vms vmcs <- get
            let (cp, ps') = getCharAndList
                  ps ((==) (y,x) . pos) noP (colorLetter $ color $ head ps)
                (cg, vhgs') = getCharAndList vhgs (== ((y,x),(y,x+1))) noG vgc
                (cvm, vms') = getCharAndList vms (== (y,x)) noP (head vmcs)
                vmcs' = if cvm /= noP then tail vmcs else vmcs
                cTile | cp /= noP = cp
                      | cvm /= noP = cvm
                      | otherwise = noP
            modify $ \s -> s { players = ps'
                             , vertHalfGates = vhgs'
                             , validMoves = vms'
                             , leftValidMovesChars = vmcs'
                             }
            tellStr [cTile,cg]
            go y (x+1)
  go row 0


renderBetweenRow :: Int -> Render ()
renderBetweenRow row = do
  bs <- reader boardSize
  let go y x
        | x == bs = void $ tellStr "\n"
        | otherwise = do
            hhgs <- gets horizHalfGates
            let (c, hhgs') = getCharAndList hhgs (== ((y,x),(y+1,x))) noG hgc
            modify $ \s -> s { horizHalfGates = hhgs' }
            tellStr (c:" ")
            go y $ x+1
  go row 0

-- | This function, checks if the first item in the sorted list
-- (of players/gates/valid moves) satisfies a predicate.
-- The predicate is basically, whether or not that item should be printed
-- in the current position of the board render.
-- It returns the according character that should be printed in that
-- potential tile, and returns the tail of the list if there's a match.
getCharAndList :: [a] -> (a -> Bool) -> Char -> Char -> (Char, [a])
getCharAndList [] _ cFalse _ = (cFalse, [])
getCharAndList (x:xs) predicate cFalse cTrue
  | predicate x = (cTrue, xs)
  | otherwise = (cFalse, x:xs)

partitionHalfGates :: [HalfGate] -> ([HalfGate],[HalfGate])
partitionHalfGates = partition $ \((_,x),(_,x')) -> x == x'

sortPlayers :: [Player] -> [Player]
sortPlayers = sortBy func
  where func p1 p2
          | pos p1 < pos p2 = LT
          | pos p1 > pos p2 = GT
          | otherwise = EQ

colorLetter :: Color -> Char
colorLetter = head . show

addColor :: String -> (String, [IO ()])
addColor str = (str, map addColorChar str)
  where addColorChar ch = CA.setSGR [CA.SetColor CA.Foreground CA.Vivid col]
          where
            col | ch == noP = CA.Yellow
                | ch == hgc || ch == vgc = CA.Magenta
                | ch `elem` validMovesChars = CA.Cyan
                | ch == 'W' = CA.White
                | ch == 'B' = CA.Blue
                | ch == 'R' = CA.Red
                | ch == 'G' = CA.Green
                | otherwise = CA.White



-- constants

noP, noG, hgc, vgc :: Char
noP = 'E'
noG = ' '
hgc = '-'
vgc = '|'

linePadding :: String
linePadding = replicate 2 ' '
