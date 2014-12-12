module Quoridor.Cmdline
where

import Quoridor.Cmdline.Render (runRender, runRenderColor, putColoredStr)
import Quoridor.Cmdline.Parse (parseTurn)
import Quoridor.Cmdline.Network (hostServer, connectClient)
import Quoridor (makeTurn, checkAndSetWinner, Game, runGame,
  Color(..), Turn, GameState(..), Player(..), currP, GameConfig(..))
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import System.Environment (getArgs)
import System.IO
import Quoridor.Cmdline.Options (getOptions, Options(..), ExecMode(..))
import Quoridor.Cmdline.Messages
import Network.Simple.TCP (withSocketsDo)



cmdlineMain :: IO ()
cmdlineMain = do
  args <- getArgs
  opts <- getOptions args
  let gc = GameConfig {
          gatesPerPlayer = opGatesPerPlayer opts,
          boardSize = opBoardSize opts,
          numOfPlayers = opNumOfPlayers opts
        }
  case opExecMode opts of
    ExLocal -> runGame play gc
    ExHost -> withSocketsDo $ runGame (hostServer $ opHostListenPort opts) gc
    ExJoin -> withSocketsDo $ connectClient $ opHostListenPort opts

play :: Game IO ()
play = do
  gc <- ask
  let go showBoard msg = do
        gs <- get
        liftIO $ putColoredStr $ runRenderColor gs gc
        liftIO $ putStrLn msg
        case winner gs of
          Just c -> liftIO $ putStrLn $ msgGameEnd c
          Nothing -> do
            strTurn <- liftIO getLine
            let eTurn = parseTurn strTurn
            case eTurn of
              Left msg -> go False msg
              Right turn -> do
                wasValid <- makeTurn turn
                go True $ if wasValid
                  then msgValidTurn (color $ currP gs) turn
                  else msgInvalidTurn
  go True msgInitialTurn
