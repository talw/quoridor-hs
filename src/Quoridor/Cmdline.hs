module Quoridor.Cmdline
where

import Quoridor.Cmdline.Render (render)
import Quoridor.Cmdline.Parse (parseTurn)
import Quoridor
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad

cmdlineMain :: IO ()
cmdlineMain = void $ runGame (play True "Good luck!")
                      defaultGameConfig
{-cmdlineMain = play initialGameState True "Good luck!"-}

{-play2 :: Bool -> String -> Game IO ()-}
{-play2 gs showBoard msg = do-}
  {-render gs-}
  {-putStrLn msg-}
  {-strTurn <- getLine-}
  {-let eTurn = parseTurn strTurn-}
  {-case eTurn of-}
    {-Left msg -> play gs False msg-}
    {-Right turn -> do-}
      {-let (wasValid, gs') = runState (makeTurn turn) gs-}
      {-play gs' True $ "last Turn was "-}
                  {-++ (if not wasValid then "in" else "")-}
                  {-++ "valid"-}

play :: Bool -> String -> Game IO ()
play showBoard msg = do
  gs <- get
  bs <- reader boardSize
  liftIO $ render bs gs
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
