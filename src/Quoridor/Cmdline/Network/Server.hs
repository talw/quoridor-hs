{-# LANGUAGE OverloadedStrings #-}

module Quoridor.Cmdline.Network.Server
  ( hostServer
  ) where

import           Control.Applicative             ((<$>))
import           Control.Concurrent              (ThreadId, forkIO, killThread,
                                                  myThreadId, threadDelay,
                                                  throwTo)
import           Control.Exception               (bracket, fromException,
                                                  handle, throw)
import           Control.Monad                   (filterM, forever, unless,
                                                  (>=>))
import           Control.Monad.Reader            (ask)
import           Control.Monad.State             (MonadIO, get, liftIO)
import qualified Data.ByteString                 as B
import           Data.List                       (find)
import           Data.Maybe                      (fromJust, fromMaybe)
import           Debug.Trace
import           System.Directory                (getCurrentDirectory)
import           System.IO                       (Handle, hClose, hFlush)
import           System.Process                  (ProcessHandle,
                                                  runInteractiveCommand,
                                                  terminateProcess,
                                                  waitForProcess)

import           Network.Simple.TCP              (HostPreference (Host),
                                                  accept, listen)
import           Network.Socket                  (isReadable)
import qualified Network.WebSockets              as WS
import qualified Network.WebSockets.Snap         as WS
import qualified Snap.Core                       as Snap
import qualified Snap.Http.Server                as Snap
import qualified Snap.Util.FileServe             as Snap
import           System.FilePath                 ((</>))

import           Paths_quoridor_hs               (getDataDir)
import           Quoridor
import           Quoridor.Cmdline.Messages
import           Quoridor.Cmdline.Network.Common
import           Quoridor.Cmdline.Parse          (parseTurn)

-- | Given a port, hosts a game server that listens
-- on the given port.
-- This returns a Game monad which should be used with runGame.
hostServer :: Int -> Int -> Game IO ()
hostServer quoriHostPort httpPort = do
  liftIO $ forkIO $ httpListen quoriHostPort httpPort
  listen (Host "127.0.0.1") (show quoriHostPort) $
    \(lstnSock, _) -> do
      gc <- ask

      let getPlayers 0 socks = do
            coSocks <- liftIO $ filterM isAliveSock socks
            if length coSocks /= length socks
              then getPlayers (length socks - length coSocks) coSocks
              else do
                let colors = map toEnum [0..]
                    connPs = zipWith ConnPlayer socks colors
                    {-getConnPlayers socks' = zipWith ConnPlayer socks' colors-}
                    {-connPs = getConnPlayers socks-}
                mapM_ (\p -> sendToPlayer (gc, coplColor p) p) connPs
                playServer connPs

          getPlayers n socks = accept lstnSock $ \(connSock, _) -> do
            let msg = "Connected. " ++ if n > 1
                  then "Waiting for other players." else ""
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
        Just _  -> liftIO $ threadDelay $ 10 * 1000 * 1000
        Nothing -> do
          let currColor = color $ currP gs
              currConnP = fromJust $ find ((currColor ==) . coplColor) connPs
              sendToCurrPlayer x = sendToPlayer x currConnP

              execValidTurn = do
                strTurn <- recvFromPlayer currConnP
                let reAskForInput msg' = do sendToCurrPlayer (gs,vm,msg')
                                            execValidTurn
                either reAskForInput
                       (makeTurn >=> maybe (reAskForInput msgInvalidTurn)
                                           return)
                       $ parseTurn strTurn

          turn <- execValidTurn
          play $ msgValidTurn currColor turn

sendToPlayer :: (Show s, MonadIO m) => s -> ConnPlayer -> m ()
sendToPlayer s cnp = sendToSock s $ coplSock cnp

-- | The error message will appear only if the current player exits.
-- To handle the case where other players will exit I'll have to rewrite the whole
-- mechanism to be asynchronous between players.
recvFromPlayer :: (Functor m, MonadIO m) => ConnPlayer -> m String
recvFromPlayer cnp = fromMaybe throwErr <$> recvFromSock (coplSock cnp)
  where throwErr = error $ "Lost connection with " ++ show (coplColor cnp)

httpListen :: Int -> Int -> IO ()
httpListen quoriHostPort httpPort = Snap.httpServe config $ app quoriHostPort
  where
    config = Snap.setPort httpPort $
             Snap.setErrorLog  Snap.ConfigNoLog $
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

    let acqRsrc = do
          (hIn, hOut, _, ph) <- runInteractiveCommand cmd
          conn <- WS.acceptRequest pending
          outT <- forkIO $ copyHandleToConn hOut conn
          tId <- myThreadId
          inT <- forkIO $ copyConnToHandle conn hIn tId
          return (hIn, hOut, ph, inT, outT)
        freeRsrc (hIn, hOut, ph, inT, outT) = do
          killThread inT
          killThread outT
          hClose hIn
          hClose hOut
          terminateProcess ph
    bracket acqRsrc freeRsrc $
      \(_,_,ph,_,_) -> waitForProcess ph

    return ()

copyHandleToConn :: Handle -> WS.Connection -> IO ()
copyHandleToConn h c = do
  bs <- B.hGetSome h 4096
  unless (B.null bs) $ do
    putStrLn $ previewStr $ "WS > " ++ show bs
    WS.sendTextData c bs
    copyHandleToConn h c
 where

copyConnToHandle :: WS.Connection -> Handle -> ThreadId -> IO ()
copyConnToHandle c h t = handle thrower $ forever $ do
  bs <- WS.receiveData c
  putStrLn $ previewStr $ "WS < " ++ show bs
  B.hPutStr h bs
  hFlush h
 where
  thrower e = throwTo t (e :: WS.ConnectionException)
  {-close e = case fromException e :: Maybe WS.ConnectionException of-}
    {-Just _                   -> traceIO "terminating process. " >> terminateProcess ph-}
    {-Nothing                  -> traceIO "rethrowing. " >> throw e-}

previewStr :: String -> String
previewStr str = prvw ++ if not $ null rst then "....."
                                        else ""
 where (prvw, rst) = splitAt 80 str

