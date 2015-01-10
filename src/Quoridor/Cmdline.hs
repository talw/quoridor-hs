module Quoridor.Cmdline
  ( cmdlineMain
  ) where

import           Control.Applicative             ((<$>))
import           Control.Monad                   (when)
import           Control.Monad.Reader            (ask)
import           Control.Monad.State             (MonadIO, get, gets, liftIO)
import           Data.List                       (sort)
import           System.Environment              (getArgs)
import           System.Exit                     (exitSuccess)

import           Network.Simple.TCP              (withSocketsDo)
import           Text.Printf                     (printf)

import           Quoridor
import           Quoridor.Cmdline.Messages       (msgGameEnd, msgInitialTurn,
                                                  msgInvalidTurn, msgValidTurn)
import           Quoridor.Cmdline.Network.Client (connectClient)
import           Quoridor.Cmdline.Network.Server (hostServer)
import           Quoridor.Cmdline.Options        (ExecMode (..), Options (..),
                                                  getOptions)
import           Quoridor.Cmdline.Parse          (parseTurn)
import           Quoridor.Cmdline.Render         (putColoredStrTerm,
                                                  runRenderColor)

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
      addr = opHostListenAddr opts
      listenPort = opHostListenPort opts
      httpListenPort = opHttpListenPort opts
  when (opExecMode opts /= ExLocal) $ do
    putStrLn $ printf "Chosen address: %s" addr
    putStrLn $ printf "Chosen port: %d" listenPort
  case opExecMode opts of
    ExLocal -> runGame playLocal gc
    ExHost  -> withSocketsDo $ runGame
      (hostServer listenPort httpListenPort) gc
    joinOrProxy  -> withSocketsDo $
      connectClient (joinOrProxy == ExProxy) addr listenPort
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
              parseSuccAct turn = do
                mTurn <- makeTurn turn
                go True $ maybe msgInvalidTurn
                                (msgValidTurn (color $ currP gs))
                                mTurn
          when showBoard renderCurrentBoard
          liftIO $ putStrLn msg
          handleWinOrTurn
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
renderBoard :: GameState -> GameConfig -> ValidMoves -> IO ()
renderBoard gs gc vms = putColoredStrTerm $ runRenderColor gs gc vms

handleParse :: MonadIO m =>
  m String -> (String -> m ()) -> (Turn -> m ()) -> m ()
handleParse getStrAct failAct succAct =
  (either failAct succAct . parseTurn) =<< getStrAct

handleWinOrTurn :: MonadIO m => (Color -> Game m ()) -> Game m () -> Game m ()
handleWinOrTurn wonAct contAct =
  maybe contAct wonAct =<< gets winner

wonAction :: MonadIO m => Color -> m ()
wonAction = liftIO . putStrLn . msgGameEnd
