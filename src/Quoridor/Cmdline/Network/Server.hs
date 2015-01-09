{-# LANGUAGE OverloadedStrings #-}

module Quoridor.Cmdline.Network.Server
  ( hostServer
  ) where

import           Control.Applicative             ((<$>))
import           Control.Concurrent              (forkFinally, forkIO,
                                                  threadDelay)
import           Control.Exception               (bracket)
import           Control.Monad                   (filterM, forM_, forever,
                                                  unless, when, zipWithM,
                                                  (>=>))
import           Control.Monad.Reader            (ask)
import           Control.Monad.State             (MonadIO, get, liftIO)
import qualified Data.ByteString                 as B
import           Data.Functor                    (void)
import           Data.List                       (find)
import           Data.Maybe                      (fromJust, fromMaybe,
                                                  isNothing, maybe)
import           System.Directory                (getCurrentDirectory)
import           System.IO                       (Handle, hClose, hFlush)
import           System.Process                  (runInteractiveCommand,
                                                  terminateProcess)

import           Control.Concurrent.Async        (race)
import           Control.Concurrent.MVar         (MVar, newEmptyMVar, putMVar,
                                                  takeMVar)
import           Control.Concurrent.STM          (STM, TChan, TVar, atomically,
                                                  newTChanIO, newTVarIO,
                                                  readTChan, readTVar,
                                                  writeTChan)
import           Network.Simple.TCP              (HostPreference (Host),
                                                  Socket, accept, listen)
import qualified Network.WebSockets              as WS
import qualified Network.WebSockets.Snap         as WS
import qualified Snap.Core                       as Snap
import qualified Snap.Http.Server                as Snap
import qualified Snap.Util.FileServe             as Snap
import           System.FilePath                 ((</>))
import           Text.Printf                     (printf)

import           Paths_quoridor_hs               (getDataDir)
import           Quoridor
import           Quoridor.Cmdline.Messages
import           Quoridor.Cmdline.Network.Common
import           Quoridor.Cmdline.Parse          (parseTurn)

type PChans = TVar [TChan Message]
type GameChan = TChan (Color, Turn)

-- | Given a port, hosts a game server that listens
-- on the given port.
-- This isn't optimal. If a player keeps on connecting and
-- disconnecting, there's no limit to the depth of the stack that is
-- possible here.
hostServer :: Int -> Int -> Game IO ()
hostServer quoriHostPort httpPort = do
  liftIO $ forkIO $ httpListen quoriHostPort httpPort
  listen (Host "127.0.0.1") (show quoriHostPort) $
    \(lstnSock, _) -> do
      gc <- ask
      gameChan <- liftIO newTChanIO
      playerChans <- liftIO $ newTVarIO []

      let
        getPlayers 0 socks = do
          coSocks <- filterM isAliveSock socks
          if length coSocks /= length socks
            then getPlayers (length socks - length coSocks) coSocks
            else do
              let colors = map toEnum [0..]
              doneConnPs <-
                zipWithM (handleClient gameChan playerChans) socks colors
              let connPs = map snd doneConnPs
              forM_ connPs $ \p ->
                sendToPlayer (FstGameMsg gc (coplColor p)) p
              return (map fst doneConnPs, connPs)

        getPlayers n socks = accept lstnSock $ \(connSock, _) -> do
          let msg = if n - 1 > 0
                then printf "Waiting for %d more players." $ show $ n - 1
                else "Game can begin."
          putStrLn msg
          sendMsg (WaitMsg msg) connSock
          getPlayers (n-1) $ connSock : socks

      (dones, connPs) <- liftIO $ getPlayers (numOfPlayers gc) []
      handleGame gameChan connPs
      -- Allow 10 seconds for the clients to shutdown
      void $ liftIO $ race (threadDelay $ 10 * 1000 * 1000)
                    (mapM_ takeMVar dones)

handleClient :: GameChan -> PChans -> Socket ->
                Color -> IO (MVar (), ConnPlayer)
handleClient gameChan playerChans sock col = do
  chan <- newTChanIO
  done <- newEmptyMVar
  let action = race (handleMsgSend chan sock)
                    (handleClientInput gameChan playerChans col sock)
      finally _ = putMVar done ()
  forkFinally action finally
  return $ (done, ConnPlayer chan col)

handleGame :: GameChan -> [ConnPlayer] -> Game IO ()
handleGame gameChan connPs = go msgInitialTurn
  where
    go msg = do
      gs <- get
      vm <- getCurrentValidMoves
      liftIO $ atomically $
        broadcast (GameMsg gs vm msg) $ map coplChan connPs
      when (isNothing $ winner gs) $ do
        let currColor = color $ currP gs
            currConnP = fromJust $ find ((currColor ==) . coplColor) connPs
            getTurnOfCurrP = do
              (col, turn) <- atomically $ readTChan gameChan
              if (col == currColor) then return turn
                                    else getTurnOfCurrP
            execValidTurn = do
              turn <- liftIO $ getTurnOfCurrP
              let reAskForInput msg' = do sendToPlayer (GameMsg gs vm msg')
                                                       currConnP
                                          execValidTurn
              makeTurn turn >>= maybe (reAskForInput msgInvalidTurn)
                                      return
        turn <- execValidTurn
        go $ msgValidTurn currColor turn

{-playServer :: [ConnPlayer] -> Game IO ()-}
{-playServer connPs = play msgInitialTurn-}
  {-where-}
    {-play msg = do-}
      {-gs <- get-}
      {-vm <- getCurrentValidMoves-}
      {-mapM_ (sendToPlayer (gs,vm,msg)) connPs-}
      {-case winner gs of-}
        {-Just _  -> liftIO $ threadDelay $ 10 * 1000 * 1000-}
        {-Nothing -> do-}
          {-let currColor = color $ currP gs-}
              {-currConnP = fromJust $ find ((currColor ==) . coplColor) connPs-}
              {-sendToCurrPlayer x = sendToPlayer x currConnP-}

              {-execValidTurn = do-}
                {-strTurn <- recvFromPlayer currConnP-}
                {-let reAskForInput msg' = do sendToCurrPlayer (gs,vm,msg')-}
                                            {-execValidTurn-}
                {-either reAskForInput-}
                       {-(makeTurn >=> maybe (reAskForInput msgInvalidTurn)-}
                                           {-return)-}
                       {-$ parseTurn strTurn-}

          {-turn <- execValidTurn-}
          {-play $ msgValidTurn currColor turn-}

sendToPlayer :: MonadIO m => Message -> ConnPlayer -> m ()
sendToPlayer msg cnp = liftIO $ atomically $ writeTChan (coplChan cnp) msg

-- | The error message will appear only if the current player exits.
-- To handle the case where other players will exit I'll have to rewrite the whole
-- mechanism to be asynchronous between players.
{-recvFromPlayer :: (Functor m, MonadIO m) => ConnPlayer -> m String-}
{-recvFromPlayer cnp = fromMaybe throwErr <$> recvFromSock (coplSock cnp)-}
  {-where throwErr = error $ "Lost connection with " ++ show (coplColor cnp)-}

handleMsgSend :: TChan Message -> Socket -> IO ()
handleMsgSend chan sock = do
  msg <- atomically $ readTChan chan
  sendMsg msg sock
  handleMsgSend chan sock

handleClientInput :: GameChan -> PChans -> Color -> Socket -> IO ()
handleClientInput gameChan playerChans col sock = go
 where
  go = do
    msg <- recvMsg sock
    atomically $ case msg of
      chatMsg@(ChatMsg _ _) -> readTVar playerChans >>= broadcast chatMsg
      TurnMsg turn          -> writeTChan gameChan (col, turn)
      _ -> error "handleClientInput - unexpected Message"
    go

broadcast :: Message -> [TChan Message] -> STM ()
broadcast msg playerChans = forM_ playerChans $ (flip writeTChan) msg


-- Web interface

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
          return (hIn, hOut, ph, conn)
        freeRsrc (hIn, hOut, ph, _) = do
          hClose hIn
          hClose hOut
          terminateProcess ph
    bracket acqRsrc freeRsrc $
      \(hIn,hOut,_,conn) -> race (copyHandleToConn hOut conn) (copyConnToHandle conn hIn)

    return ()

copyHandleToConn :: Handle -> WS.Connection -> IO ()
copyHandleToConn h c = do
  bs <- B.hGetSome h 4096
  unless (B.null bs) $ do
    putStrLn $ previewStr $ "WS > " ++ show bs
    WS.sendTextData c bs
    copyHandleToConn h c
 where

copyConnToHandle :: WS.Connection -> Handle -> IO ()
copyConnToHandle c h = forever $ do
  bs <- WS.receiveData c
  putStrLn $ previewStr $ "WS < " ++ show bs
  B.hPutStr h bs
  hFlush h

previewStr :: String -> String
previewStr str = prvw ++ if not $ null rst then "....."
                                        else ""
 where (prvw, rst) = splitAt 80 str

