module Quoridor.Cmdline
  ( cmdlineMain
  ) where

import           Control.Applicative       ((<$>))
import           Control.Monad             (when)
import           Control.Monad.Reader      (ask)
import           Control.Monad.State       (MonadIO, get, liftIO)
import           Data.List                 (sort)
import           System.Environment        (getArgs)
import           System.Exit               (exitSuccess)

import           Network.Simple.TCP        (withSocketsDo)

import           Quoridor
import           Quoridor.Cmdline.Messages (msgGameEnd, msgInitialTurn,
                                            msgInvalidTurn, msgValidTurn)
import           Quoridor.Cmdline.Network  (connectClient, hostServer)
import           Quoridor.Cmdline.Options  (ExecMode (..), Options (..),
                                            getOptions)
import           Quoridor.Cmdline.Parse    (parseTurn)
import           Quoridor.Cmdline.Render   (putColoredStr, runRenderColor)

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

-- playLocal used to be structured similarly to
-- playClient and playServer, but those functions
-- seemed a bit monolithc to me, so I tried an alternative
-- approach that, admittedly, I'm not sure is more readable.
playLocal :: Game IO ()
playLocal = go True msgInitialTurn
  where go showBoard msg = do
          gs <- get
          let parseFailAct = go False
              parseSuccAct turn = do wasValid <- makeTurn turn
                                     go True $ if wasValid
                                       then msgValidTurn (color $ currP gs) turn
                                       else msgInvalidTurn

          when showBoard renderCurrentBoard
          liftIO $ putStrLn msg
          handleWinOrTurn gs
            wonAction $
            handleParse (liftIO getLine) parseFailAct parseSuccAct

renderCurrentBoard :: (Functor m, MonadIO m) => Game m ()
renderCurrentBoard = do
  gc <- ask
  gs <- get
  vm <- sort <$> getCurrentValidMoves
  liftIO $ renderBoard gs gc vm

-- Function that could be common to playLocal,
-- playClient(handleWinOrTurn, wonAction, renderBoard) and playServer(handleParse)
-- if I change playClient and playServer to be in the same vein as playLocal
renderBoard :: GameState -> GameConfig -> [Cell] -> IO ()
renderBoard gs gc vms = putColoredStr $ runRenderColor gs gc vms

handleParse :: MonadIO m =>
  m String -> (String -> m ()) -> (Turn -> m ()) -> m ()
handleParse strAct failAct succAct = do
  strTurn <- strAct
  case parseTurn strTurn of
    Left msg   -> failAct msg
    Right turn -> succAct turn

handleWinOrTurn :: MonadIO m => GameState -> (Color -> m ()) -> m () -> m ()
handleWinOrTurn gs wonAct contAct =
  case winner gs of
    Just c  -> wonAct c
    Nothing -> contAct

wonAction :: MonadIO m => Color -> m ()
wonAction = liftIO . putStrLn . msgGameEnd
