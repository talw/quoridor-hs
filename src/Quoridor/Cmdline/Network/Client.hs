module Quoridor.Cmdline.Network.Client
  ( connectClient
  ) where

import           Control.Applicative             ((<$>))
import           Control.Monad                   (when)
import           Control.Monad.State             (MonadIO, liftIO)
import           Data.Maybe                      (fromMaybe)
import           System.IO                       (BufferMode (LineBuffering),
                                                  hFlush, hReady,
                                                  hSetBuffering, stdin, stdout)

import           Network.Simple.TCP              (Socket, connect)

import           Quoridor
import           Quoridor.Cmdline.Messages
import           Quoridor.Cmdline.Network.Common
import           Quoridor.Cmdline.Render         (putColoredStrHtml,
                                                  putColoredStrTerm,
                                                  runRenderColor)


-- | Given a port, joins a game server that listens
-- on the given port.
connectClient :: Bool -> String -> Int -> IO ()
connectClient isProxy addr port = connect addr (show port) $
  \(connSock, _) -> do
    -- In case the client is invoked as a proxy
    -- because the default is BlockBuffering
    hSetBuffering stdout LineBuffering
    msg <- recvFromServer connSock
    putStrLn msg
    (gc, c) <- recvFromServer connSock
    playClient connSock isProxy gc c

playClient :: Socket -> Bool -> GameConfig -> Color -> IO ()
playClient connSock isProxy gc myColor = play
  where
    play = do
      (gs, vm, msg) <- recvFromServer connSock
      emptyInput
      (if isProxy then putColoredStrHtml else putColoredStrTerm) $
        runRenderColor gs gc vm
      putStrLn msg
      hFlush stdout
      case winner gs of
        Just c  ->
          putStrLn $ msgGameEnd c
        Nothing -> do
          let currPC = color $ currP gs
          if currPC /= myColor
            then do
              putStrLn $ msgAwaitingTurn currPC
              play
            else do
              strTurn <- liftIO getLine
              sendToSock strTurn connSock
              play

recvFromServer :: (Functor m, MonadIO m, Read r) => Socket -> m r
recvFromServer sock = fromMaybe throwErr <$> recvFromSock sock
  where throwErr = error "Lost connection with the server"

-- | This empties command line input that was buffered
-- while it wasn't the player's turn.
-- Otherwise, garbage that was being fed to input
-- while it wasn't the player's turn, will
-- be fed to the server and generate an error message per line,
-- or even play a turn, which may or may not be intentional.
emptyInput :: IO ()
emptyInput = do
  inputExists <- hReady stdin
  when inputExists $ getLine >> emptyInput

