module Quoridor.Cmdline
where

import Quoridor.Cmdline.Render (runRender)
import Quoridor.Cmdline.Parse (parseTurn)
import Quoridor (makeTurn, getWinner, Game, gameConfig, runGame)
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import System.Environment (getArgs)
import System.IO
import Quoridor.Cmdline.Options (getSettings, Settings(..))

cmdlineMain :: IO ()
cmdlineMain = do
  args <- getArgs
  settings <- getSettings args
  void $ runGame (play stdin) $
    gameConfig (gatesPerPlayer settings) (boardSize settings)

{-runGameFromScript :: IO ()-}
{-runGameFromScript = do-}
  {-handle <- openFile "moves" ReadMode-}
  {-void $ runGame (play handle) defaultGameConfig-}

{-playLocalGame :: IO ()-}
{-playLocalGame = void $ runGame (play stdin) defaultGameConfig-}

play :: Handle -> Game IO ()
play h = do
  gc <- ask
  let go showBoard msg = do
        gs <- get
        liftIO $ putStr $ runRender gs gc
        liftIO $ putStrLn msg
        mColor <- getWinner
        case mColor of
          Just c -> liftIO $ putStrLn $ show c ++ " won!"
          Nothing -> do
            strTurn <- liftIO $ hGetLine h
            let eTurn = parseTurn strTurn
            case eTurn of
              Left msg -> go False msg
              Right turn -> do
                wasValid <- makeTurn turn
                go True $ "last Turn was "
                            ++ (if not wasValid then "in" else "")
                            ++ "valid"
  go True "Good luck!"
