module Quoridor.Cmdline.Network
  ( hostServer
  , connectClient
  ) where

import           Control.Monad         (when)
import           Control.Monad.Reader  (ask)
import           Control.Monad.State   (MonadIO, get, liftIO)
import qualified Data.ByteString.Char8 as B
import           Data.List             (find)
import           Data.Maybe            (fromJust)
import           System.IO             (hReady, stdin)
import           Text.Printf           (printf)

import Network.Simple.TCP (HostPreference (Host), Socket, accept, connect,
                           listen, recv, send)
import Numeric            (readHex, showHex)

import Quoridor
import Quoridor.Cmdline.Messages (msgAwaitingTurn, msgGameEnd, msgInitialTurn,
                                  msgInvalidTurn, msgValidTurn)
import Quoridor.Cmdline.Parse    (parseTurn)
import Quoridor.Cmdline.Render   (putColoredStr, runRenderColor)



-- Common of Server and Client

data ConnPlayer = ConnPlayer
  { coplSock  :: Socket
  , coplColor :: Color
  } deriving Show

sendToSock :: (Show s, MonadIO m) => s -> Socket -> m ()
sendToSock s sock = do
    send sock $ B.pack $
      printf "%04s" $ showHex (B.length serialized) ""
    send sock serialized
  where serialized = B.pack $ show s

recvFromSock :: (Read r, MonadIO m) => Socket -> m r
recvFromSock sock = do
  mHexSize <- recv sock 4
  let ((size,_):_) = readHex $ B.unpack $ fromJust mHexSize
  mValue <- recv sock size
  return $ read $ B.unpack $ fromJust mValue



-- Server

-- | Given a port, hosts a game server that listens
-- on the given port.
-- This returns a Game monad which should be used with runGame.
hostServer :: Int -> Game IO ()
hostServer portStr = listen (Host "127.0.0.1") (show portStr) $
  \(lstnSock, _) -> do
    gc <- ask
    let getPlayers 0 socks = do
          let colors = map toEnum [0..]
              getConnPlayers socks' = zipWith ConnPlayer socks' colors
              connPs = getConnPlayers socks
          mapM_ (\p -> sendToPlayer (gc, coplColor p) p) connPs
          playServer connPs
        getPlayers n socks = accept lstnSock $ \(connSock, _) -> do
          let msg = "Connected. " ++ if n > 1
                then "Waiting for other players."
                else "Game begins."
          liftIO $ putStrLn msg
          sendToSock msg connSock
          getPlayers (n-1) $ connSock : socks
    getPlayers (numOfPlayers gc) []

playServer :: [ConnPlayer] -> Game IO ()
playServer connPs = play msgInitialTurn
  where
    play msg = do
      gs <- get
      vm <- getCurrentValidMoves
      mapM_ (sendToPlayer (gs,vm,msg)) connPs
      case winner gs of
        Just _  -> return ()
        Nothing -> do
          let currColor = color $ currP gs
              currConnP = fromJust $ find ((currColor ==) . coplColor) connPs
              sendToCurrPlayer x = sendToPlayer x currConnP
              execValidTurn = do
                strTurn <- recvFromPlayer currConnP
                let eTurn = parseTurn strTurn
                case eTurn of
                  Left invalidParseMsg -> do
                    sendToCurrPlayer (gs,invalidParseMsg)
                    execValidTurn
                  Right turn -> do
                    wasValid <- makeTurn turn
                    if wasValid
                      then return turn
                      else do
                        sendToCurrPlayer (gs, msgInvalidTurn)
                        execValidTurn
          turn <- execValidTurn
          play $ msgValidTurn currColor turn

sendToPlayer :: (Show s, MonadIO m) => s -> ConnPlayer -> m ()
sendToPlayer s cnp = sendToSock s $ coplSock cnp

recvFromPlayer :: MonadIO m => ConnPlayer -> m String
recvFromPlayer cnp = recvFromSock $ coplSock cnp



-- Client

-- | Given a port, joins a game server that listens
-- on the given port.
connectClient :: Int -> IO ()
connectClient portStr = connect "127.0.0.1" (show portStr) $
  \(connSock, _) -> do
    msg <- recvFromSock connSock
    putStrLn msg
    (gc, c) <- recvFromSock connSock
    playClient connSock gc c

playClient :: Socket -> GameConfig -> Color -> IO ()
playClient connSock gc myColor = play
  where
    play = do
      (gs, vm, msg) <- recvFromSock connSock
      flushInput
      putColoredStr $ runRenderColor gs gc vm
      putStrLn msg
      case winner gs of
        Just c  -> liftIO $ putStrLn $ msgGameEnd c
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

-- | This flushes command line input that was buffered while it wasn't the player's turn
-- Otherwise, garbage that was being fed to input while it wasn't the player's turn, will
-- be fed to the server and generate an error message per line, or even play a turn,
-- which may or may not be intentional.
flushInput :: IO ()
flushInput = do
  inputExists <- hReady stdin
  when inputExists $ getLine >> flushInput
