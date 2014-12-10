module Quoridor.Cmdline.Render (runRender)
where

import Quoridor
import Data.List (sortBy, partition)
import qualified Data.Set as S (toAscList)
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import qualified Data.DList as D

type Render = ReaderT GameConfig
                (StateT RenderState
                (Writer (D.DList Char)))

data RenderState = RenderState {
  players :: [Player],
  vertHalfGates :: [HalfGate],
  horizHalfGates :: [HalfGate]
}

--- exported functions

runRender :: GameState -> GameConfig -> String
runRender gs gc = D.toList w
  where (_,w) =
          runWriter (runStateT (runReaderT (render cp) gc) initialRenderState)
        initialRenderState = RenderState ps vhgs hhgs
        ps = sortPlayers $ playerList gs
        (hhgs, vhgs) = partitionHalfGates $ S.toAscList $ halfGates gs
        cp = currP gs



--- helper functions

render ::  Player -> Render ()
render cp = do
  renderBoard
  tellLine "type    g y x [h|v]   to place horizontal/vertical gate."
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
                  charAndList isPlayerHere noP (head $ show $ color p) ps
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
partitionHalfGates = partition $ \((y,x),(y',x')) -> x == x'

sortPlayers :: [Player] -> [Player]
sortPlayers = sortBy func
  where func p1 p2
          | pos p1 < pos p2 = LT
          | pos p1 > pos p2 = GT
          | otherwise = EQ

headOrDefault :: a -> [a] -> a
headOrDefault x [] = x
headOrDefault _ (x:xs) = x

charAndList :: Bool -> Char -> Char -> [a] -> (Char, [a])
charAndList b cFalse cTrue list = if b then (cTrue, tail list)
                                       else (cFalse, list)



-- constants

dp :: Player
dp = Player Black (-1, -1) 0
dg :: ((Int, Int),(Int, Int))
dg = ((-1,-1),(-1,-1))

noP, noG, hgc, vgc :: Char
noP = 'e'
noG = ' '
hgc = '-'
vgc = '|'

linePadding :: String
linePadding = replicate 2 ' '
