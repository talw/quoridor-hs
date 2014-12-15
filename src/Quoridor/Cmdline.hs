module Quoridor.Cmdline
  ( cmdlineMain
  ) where

import Control.Applicative  ((<$>))
import Control.Monad        (when)
import Control.Monad.Reader (ask)
import Control.Monad.State  (get, liftIO)
import Data.List            (sort)
import System.Environment   (getArgs)
import System.Exit          (exitSuccess)

import Network.Simple.TCP (withSocketsDo)

import Quoridor
import Quoridor.Cmdline.Messages (msgGameEnd, msgInitialTurn, msgInvalidTurn,
                                  msgValidTurn)
import Quoridor.Cmdline.Network  (connectClient, hostServer)
import Quoridor.Cmdline.Options  (ExecMode (..), Options (..), getOptions)
import Quoridor.Cmdline.Parse    (parseTurn)
import Quoridor.Cmdline.Render   (putColoredStr, runRenderColor)

-- | The main entry point to quoridor-exec
cmdlineMain :: IO ()
cmdlineMain = do
  args <- getArgs
  opts <- getOptions args
  let gc = GameConfig
          { gatesPerPlayer = opGatesPerPlayer opts
          , boardSize      = opBoardSize opts
          , numOfPlayers   = opNumOfPlayers opts
          }
  case opExecMode opts of
    ExLocal -> runGame playLocal gc
    ExHost  -> withSocketsDo $ runGame (hostServer $ opHostListenPort opts) gc
    ExJoin  -> withSocketsDo $ connectClient $ opHostListenPort opts
  exitSuccess

playLocal :: Game IO ()
playLocal = do
  gc <- ask
  let go showBoard msg = do
        gs <- get
        vm <- sort <$> getCurrentValidMoves
        when showBoard $ liftIO $ renderBoard gs gc vm
        liftIO $ putStrLn msg
        case winner gs of
          Just c  -> liftIO $ putStrLn $ msgGameEnd c
          Nothing -> do
            strTurn <- liftIO getLine
            let eTurn = parseTurn strTurn
            case eTurn of
              Left invalidParseMsg -> go False invalidParseMsg
              Right turn -> do
                wasValid <- makeTurn turn
                go True $ if wasValid
                  then msgValidTurn (color $ currP gs) turn
                  else msgInvalidTurn
  go True msgInitialTurn

renderBoard :: GameState -> GameConfig -> [Cell] -> IO ()
renderBoard gs gc vms = putColoredStr $ runRenderColor gs gc vms
