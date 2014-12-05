module Quoridor.Cmdline
where

import Quoridor.Cmdline.Render (runRender)
import Quoridor.Cmdline.Parse (parseTurn)
import Quoridor
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad

cmdlineMain :: IO ()
cmdlineMain = void $ runGame (play True "Good luck!")
                      defaultGameConfig

play :: Bool -> String -> Game IO ()
play showBoard msg = do
  gs <- get
  gc <- ask
  liftIO $ putStr $ runRender gs gc
  liftIO $ putStrLn msg
  strTurn <- liftIO getLine
  let eTurn = parseTurn strTurn
  case eTurn of
    Left msg -> play False msg
    Right turn -> do
      wasValid <- makeTurn turn
      play True $ "last Turn was "
                  ++ (if not wasValid then "in" else "")
                  ++ "valid"
