module Quoridor.Cmdline
where

import Quoridor.Cmdline.Render (render)
import Quoridor.Cmdline.Parse (parseTurn)
import Quoridor
import Control.Monad.State

cmdlineMain :: IO ()
cmdlineMain = play initialGameState True "Good luck!"

play :: GameState -> Bool -> String -> IO ()
play gs showBoard msg = do
  render gs
  putStrLn msg
  strTurn <- getLine
  let eTurn = parseTurn strTurn
  case eTurn of
    Left msg -> play gs False msg
    Right turn -> do
      let (wasValid, gs') = runState (makeTurn turn) gs
      play gs' True $ "last Turn was "
                  ++ (if not wasValid then "in" else "")
                  ++ "valid"
