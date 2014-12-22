{-# LANGUAGE OverloadedStrings #-}

module Quoridor.Cmdline.Network
  ( hostServer
  , connectClient
  ) where

import           Control.Concurrent        (forkIO)
import           Control.Exception         (fromException, handle, throw)
import           Control.Monad             (forever, unless, when)
import           Control.Monad.Reader      (ask)
import           Control.Monad.State       (MonadIO, get, liftIO)
import qualified Data.ByteString           as B
import qualified Data.ByteString.Char8     as BC
import           Data.List                 (find)
import           Data.Maybe                (fromJust)
import           System.Directory          (getCurrentDirectory)
import           System.IO                 (Handle, hClose, hFlush, hReady,
                                            stdin, stdout)
import           System.Process            (runInteractiveCommand,
                                            waitForProcess)
import           Text.Printf               (printf)

import           Network.Simple.TCP        (HostPreference (Host), Socket,
                                            accept, connect, listen, recv,
                                            send)
import qualified Network.WebSockets        as WS
import qualified Network.WebSockets.Snap   as WS
import           Numeric                   (readHex, showHex)
import qualified Snap.Core                 as Snap
import qualified Snap.Http.Server          as Snap
import qualified Snap.Util.FileServe       as Snap
import           System.FilePath           ((</>))

import           Paths_quoridor_hs         (getDataDir)
import           Quoridor
import           Quoridor.Cmdline.Messages (msgAwaitingTurn, msgGameEnd,
                                            msgInitialTurn, msgInvalidTurn,
                                            msgValidTurn)
import           Quoridor.Cmdline.Parse    (parseTurn)
import           Quoridor.Cmdline.Render   (putColoredStrHtml,
                                            putColoredStrTerm, runRenderColor)



-- Common of Server and Client

data ConnPlayer = ConnPlayer
  { coplSock  :: Socket
  , coplColor :: Color
  } deriving Show

sendToSock :: (Show s, MonadIO m) => s -> Socket -> m ()
sendToSock s sock = do
    send sock $ BC.pack $
      printf "%04s" $ showHex (BC.length serialized) ""
    send sock serialized
  where serialized = BC.pack $ show s

recvFromSock :: (Read r, MonadIO m) => Socket -> m r
recvFromSock sock = do
  mHexSize <- recv sock 4
  let ((size,_):_) = readHex $ BC.unpack $ fromJust mHexSize
  mValue <- recv sock size
  return $ read $ BC.unpack $ fromJust mValue



-- Server

-- | Given a port, hosts a game server that listens
-- on the given port.
-- This returns a Game monad which should be used with runGame.
hostServer :: Int -> Game IO ()
hostServer port = do
  liftIO $ forkIO $ httpListen port
  listen (Host "127.0.0.1") (show port) $
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

httpListen :: Int -> IO ()
httpListen = Snap.httpServe config . app
  where
    config = Snap.setErrorLog  Snap.ConfigNoLog $
             Snap.setAccessLog Snap.ConfigNoLog
             Snap.defaultConfig
    app :: Int -> Snap.Snap ()
    app port = do
      dataDir <- liftIO getDataDir
      Snap.route
        [ ("",           Snap.ifTop $ Snap.serveFile $ dataDir </> "console.html")
        , ("console.js", Snap.serveFile $ dataDir </> "console.js")
        , ("style.css",  Snap.serveFile $ dataDir </> "style.css")
        , ("play",       acceptWSPlayer port)
        ]

acceptWSPlayer :: Int -> Snap.Snap ()
acceptWSPlayer port = WS.runWebSocketsSnap $ \pending ->
  do
    dir <- getCurrentDirectory
    let cmd = dir </> "quoridor-exec -p " ++ show port
    putStrLn cmd
    (hIn, hOut, _, h) <- runInteractiveCommand cmd
    conn <- WS.acceptRequest pending
    _ <- forkIO $ copyHandleToConn hOut conn
    _ <- forkIO $ copyConnToHandle conn hIn
    _ <- waitForProcess h
    return ()

copyHandleToConn :: Handle -> WS.Connection -> IO ()
copyHandleToConn h c = do
  bs <- B.hGetSome h 4096
  unless (B.null bs) $ do
    putStrLn $ "> " ++ show bs
    WS.sendTextData c bs
    copyHandleToConn h c

copyConnToHandle :: WS.Connection -> Handle -> IO ()
copyConnToHandle c h = handle close $ forever $ do
    bs <- WS.receiveData c
    putStrLn $ "< " ++ show bs
    B.hPutStr h bs
    hFlush h
  where
    close e = case fromException e of
      Just WS.ConnectionClosed -> hClose h
      Just _                   -> hClose h
      Nothing                  -> throw e

-- Client

-- | Given a port, joins a game server that listens
-- on the given port.
connectClient :: Bool -> Int -> IO ()
connectClient isProxy port = connect "127.0.0.1" (show port) $
  \(connSock, _) -> do
    msg <- recvFromSock connSock
    flushStrLn msg
    (gc, c) <- recvFromSock connSock
    playClient connSock isProxy gc c

playClient :: Socket -> Bool -> GameConfig -> Color -> IO ()
playClient connSock isProxy gc myColor = play
  where
    play = do
      (gs, vm, msg) <- recvFromSock connSock
      emptyInput
      (if isProxy then putColoredStrHtml else putColoredStrTerm) $
        runRenderColor gs gc vm
      flushStrLn msg
      hFlush stdout
      case winner gs of
        Just c  ->
          flushStrLn $ msgGameEnd c
        Nothing -> do
          let currPC = color $ currP gs
          if currPC /= myColor
            then do
              flushStrLn $ msgAwaitingTurn currPC
              play
            else do
              strTurn <- liftIO getLine
              sendToSock strTurn connSock
              play

-- | Like putStrLn, but flushes right afterwards.
flushStrLn :: String -> IO ()
flushStrLn = (hFlush stdout <<) . putStrLn
  where (<<) = flip (>>)

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
