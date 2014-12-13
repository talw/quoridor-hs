module Quoridor.Cmdline.Render
  ( runRender
  , runRenderColor
  , putColoredStr
  ) where

import           Control.Monad.Reader (ReaderT, reader, runReaderT)
import           Control.Monad.State  (StateT, gets, modify, runStateT)
import           Control.Monad.Writer (Writer, runWriter, tell, void)
import           Data.List            (partition, sortBy)
import qualified Data.Set             as S (toAscList)

import qualified Data.DList          as D
import qualified System.Console.ANSI as CA

import Quoridor



type Render = ReaderT GameConfig
                (StateT RenderState
                (Writer (D.DList Char)))

data RenderState = RenderState
  { players        :: [Player]
  , vertHalfGates  :: [HalfGate]
  , horizHalfGates :: [HalfGate]
  }

--- exported functions

-- | Returns a String of the game board along with some basic info
runRender :: GameState -> GameConfig -> String
runRender gs gc = D.toList w
  where (_,w) =
          runWriter (runStateT (runReaderT (render cp) gc) initialRenderState)
        initialRenderState = RenderState ps vhgs hhgs
        ps = sortPlayers $ playerList gs
        (hhgs, vhgs) = partitionHalfGates $ S.toAscList $ halfGates gs
        cp = currP gs

-- | Returns a String of the game board along with some basic info,
-- and a series of IO () actions, one per character, which describe how
-- to set the terminal color. putColoredStr can be used to apply
-- those actions automatically
runRenderColor :: GameState -> GameConfig -> (String, [IO ()])
runRenderColor = (addColor .) . runRender

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
  tellLine "type    g y x [h/v]   to place horizontal/vertical gate."
  tellLine "type    m y x       to move."
  tellNewLine
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
            ps <- gets players
            vhgs <- gets vertHalfGates
            let p = headOrDefault dp ps
                vhg = headOrDefault dg vhgs
                isPlayerHere = pos p == (y,x)
                isGateHere = vhg == ((y,x),(y,x+1))
                (cp, ps') =
                  charAndList isPlayerHere noP (playerColorLetter $ color p) ps
                (cg, vhgs') = charAndList isGateHere noG vgc vhgs
            modify $ \s -> s { players = ps', vertHalfGates = vhgs' }
            tellStr [cp,cg]
            go y (x+1)
  go row 0

renderBetweenRow :: Int -> Render ()
renderBetweenRow row = do
  bs <- reader boardSize
  let go y x
        | x == bs = void $ tellStr "\n"
        | otherwise = do
            hhgs <- gets horizHalfGates
            let isGateHere = headOrDefault dg hhgs == ((y,x),(y+1,x))
                (c, hhgs') = charAndList isGateHere noG hgc hhgs
            modify $ \s -> s { horizHalfGates = hhgs' }
            tellStr (c:" ")
            go y $ x+1
  go row 0

partitionHalfGates :: [HalfGate] -> ([HalfGate],[HalfGate])
partitionHalfGates = partition $ \((_,x),(_,x')) -> x == x'

sortPlayers :: [Player] -> [Player]
sortPlayers = sortBy func
  where func p1 p2
          | pos p1 < pos p2 = LT
          | pos p1 > pos p2 = GT
          | otherwise = EQ

playerColorLetter :: Color -> Char
playerColorLetter = head . show

headOrDefault :: a -> [a] -> a
headOrDefault x [] = x
headOrDefault _ (x:_) = x

charAndList :: Bool -> Char -> Char -> [a] -> (Char, [a])
charAndList b cFalse cTrue list = if b then (cTrue, tail list)
                                       else (cFalse, list)

addColor :: String -> (String, [IO ()])
addColor str = (str, map addColorChar str)
  where addColorChar ch = CA.setSGR [CA.SetColor CA.Foreground CA.Vivid col]
          where
            col
              | ch == noP = CA.Yellow
              | ch == hgc || ch == vgc = CA.Magenta
              | ch == 'W' = CA.White
              | ch == 'B' = CA.Blue
              | ch == 'R' = CA.Red
              | ch == 'G' = CA.Green
              | otherwise = CA.White



-- constants

dp :: Player
dp = Player Black (-1, -1) 0
dg :: ((Int, Int),(Int, Int))
dg = ((-1,-1),(-1,-1))

noP, noG, hgc, vgc :: Char
noP = 'E'
noG = ' '
hgc = '-'
vgc = '|'

linePadding :: String
linePadding = replicate 2 ' '
