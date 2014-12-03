module Quoridor.Cmdline.Render (render)
where

import Quoridor
import Data.List (sortBy, partition)
import qualified Data.Set as S (toAscList)
import Control.Monad

--- exported functions

render :: Int -> GameState -> IO ()
render bs gs = do
  renderBoard bs gs
  let p = currP gs
  putStrLn $ "It's " ++ show (color p) ++ "'s Turn."
    ++ " " ++ show (gatesLeft p) ++ " gates left."
  putStrLn "type    g y x [h|v]   to place horizontal/vertical gate."
  putStrLn "type    m y x       to move."
  putNewLine



--- helper functions

putNewLine :: IO ()
putNewLine = putChar '\n'

renderBoard :: Int -> GameState -> IO ()
renderBoard bs gs = do
  putRulerLine
  putNewLine
  go bs 0 (sortPlayers $ playerList gs) hhgs vhgs
  putRulerLine
  putNewLine
    where putRulerLine = putStrLn $
            linePadding ++ unwords (map show [0..bs-1])
          (hhgs, vhgs) = partitionHalfGates $ S.toAscList $ halfGates gs
          go bs y ps hhgs vhgs
            | y == bs = return ()
            | otherwise = do
              let lineRuler = show y ++ tail linePadding
              (ps', vhgs') <- putStr lineRuler >> renderTileRow bs y ps vhgs
              hhgs' <- putStr linePadding >> renderBetweenRow bs y hhgs
              go bs (y+1) ps' hhgs' vhgs'

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

renderTileRow :: Int -> Int -> [Player] -> [HalfGate] -> IO ([Player],[HalfGate])
renderTileRow bs row = go row 0
  where
    go y x ps vhgs
      | x == bs = putChar '\n' >> return (ps, vhgs)
      | otherwise = do
          let p = headOrDefault dp ps
              vhg = headOrDefault dg vhgs
              isPlayerHere = pos p == (y,x)
              isGateHere = vhg == ((y,x),(y,x+1))
              (cp, ps') = charAndList isPlayerHere noP (head $ show $ color p) ps
              (cg, vhgs') = charAndList isGateHere noG vgc vhgs
          putStr [cp,cg]
          go y (x+1) ps' vhgs'

renderBetweenRow :: Int -> Int -> [HalfGate] -> IO [HalfGate]
renderBetweenRow bs row = go row 0
  where go y x hhgs
          | x == bs = putChar '\n' >> return hhgs
          | otherwise = do
              let isGateHere = headOrDefault dg hhgs == ((y,x),(y+1,x))
                  (c, hhgs') = charAndList isGateHere noG hgc hhgs
              putStr (c:" ")
              go y (x+1) hhgs'



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
