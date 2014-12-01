module Quoridor.Cmdline.Render (render, someGameState)
where

import Quoridor
import Data.List (sortBy, partition)
import qualified Data.Set as S (toAscList)
import Control.Monad

render :: GameState -> IO ()
render gs = do
  renderBoard gs
  let p = currP gs
  putStrLn $ "It's " ++ show (color p) ++ "'s Turn."
  putStrLn "type    g (y,x) h/v   to place horizontal/vertical gate."
  putStrLn "type    m (y,x)       to move."
  newLine

newLine :: IO ()
newLine = putChar '\n'

renderBoard :: GameState -> IO ()
renderBoard gs = do
  putStrLn $ "  " ++ unwords (map show [0..boardSize-1])
  newLine
  go 0 (sortPlayers $ playerList gs) hhgs vhgs
    where (hhgs, vhgs) = partitionHalfGates $ S.toAscList $ halfGates gs
          go y ps hhgs vhgs
            | y == boardSize = return ()
            | otherwise = do
              let linePrefix = show y ++ " "
              (ps', vhgs') <- putStr linePrefix >> renderTileRow y ps vhgs
              hhgs' <- renderBetweenRow y hhgs
              go (y+1) ps' hhgs' vhgs'

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

renderTileRow :: Int -> [Player] -> [HalfGate] -> IO ([Player],[HalfGate])
renderTileRow row = go row 0
  where
    go y x ps vhgs
      | x == boardSize = putChar '\n' >> return (ps, vhgs)
      | otherwise = do
          let p = headOrDefault dp ps
              vhg = headOrDefault dg vhgs
              isPlayerHere = pos p == (y,x)
              isGateHere = fst vhg == (y,x)
              (cp, ps') = charAndList isPlayerHere noP (head $ show $ color p) ps
              (cg, vhgs') = charAndList isGateHere noG vgc vhgs
          putStr [cp,cg]
          go y (x+1) ps' vhgs'

renderBetweenRow :: Int -> [HalfGate] -> IO [HalfGate]
renderBetweenRow row = go row 0
  where go y x hhgs
          | x == boardSize = putChar '\n' >> return hhgs
          | otherwise = do
              let isGateHere = headOrDefault dg hhgs == ((y,x),(y+1,x))
                  (c, hhgs') = charAndList isGateHere noG hgc hhgs
              putStr (c:" ")
              go y (x+1) hhgs'

someGameState :: GameState
someGameState = initialGameState {
                  halfGates = halfGates',
                  playerLoop = concat $ repeat playerList'
                }
  where halfGates' = foldr insertGate (halfGates initialGameState) [
            gateUpperLeft (2,3) H
          , gateUpperLeft (2,4) V
          ]
        playerList' = [
            Player { color = Black,
                     pos = (3,3),
                     gatesLeft = 1 }
          , Player { color = White,
                     pos = (3,4),
                     gatesLeft = 0 }
          ]


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
