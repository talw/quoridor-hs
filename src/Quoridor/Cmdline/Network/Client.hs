{-# LANGUAGE BangPatterns #-}

module Quoridor.Cmdline.Network.Client
  ( connectClient
  ) where

import           Control.Concurrent.Async        (race)
import           Control.Concurrent.MVar         (MVar, newMVar, withMVar)
import           Control.Monad                   (unless, when)
import           Data.Functor                    (void)
import           Data.Maybe                      (isJust)
import           System.IO                       (BufferMode (LineBuffering),
                                                  hFlush, hReady,
                                                  hSetBuffering, stdin, stdout)

import           Network.Simple.TCP              (Socket, connect)

import           Quoridor
import           Quoridor.Cmdline.Messages
import           Quoridor.Cmdline.Network.Common
import           Quoridor.Cmdline.Parse          (parseMessage)
import           Quoridor.Cmdline.Render         (putChatMessageJson,
                                                  putColoredBoardHtml,
                                                  putColoredBoardTerm,
                                                  runRenderColor)


-- | Given an address, joins a game server that listens
-- on that address.
connectClient :: Bool -> String -> Int -> IO ()
connectClient isProxy addr port = connect addr (show port) $
  \(sock, _) -> do
    -- In case the client is invoked as a proxy
    -- because the default is BlockBuffering
    hSetBuffering stdout LineBuffering
    let go = do
          msg <- recvMsg sock
          case msg of
            WaitMsg str -> flushStrLn str >> go
            FstGameMsg gc c -> do
              writeLock <- newMVar ()
              void $ race (handleServerInput isProxy gc c sock writeLock)
                          (handleUserInput c sock writeLock)
            _ -> error "connectClient - unexpected Message"
    go

    {-(gc, c) <- recvFromServer connSock-}
    {-playClient connSock isProxy gc c-}

{-playClient :: Socket -> Bool -> GameConfig -> Color -> IO ()-}
{-playClient connSock isProxy gc myColor = play-}
  {-where-}
    {-play = do-}
      {-(gs, vm, msg) <- recvFromServer connSock-}
      {-[>emptyInput<] -- unneccessary if we're concurrent-}
      {-(if isProxy then putColoredStrHtml-}
                  {-else (putChar '\n' >>) . putColoredStrTerm) $-}
        {-runRenderColor gs gc vm-}
      {-flushStrLn msg-}
      {-hFlush stdout-}
      {-case winner gs of-}
        {-Just c  ->-}
          {-flushStrLn $ msgGameEnd c-}
        {-Nothing -> do-}
          {-let currPC = color $ currP gs-}
          {-if currPC /= myColor-}
            {-then do-}
              {-flushStrLn $ msgAwaitingTurn currPC-}
              {-play-}
            {-else do-}
              {-strTurn <- getLine-}
              {-sendToSock strTurn connSock-}
              {-play-}

handleServerInput :: Bool -> GameConfig -> Color -> Socket -> MVar () -> IO ()
handleServerInput isProxy gc myColor sock writeLock = go
 where
  go = do
    msg <- recvMsg sock
    quit <- case msg of
      GameMsg gs vm str -> handleGameMsg gs vm str
      ChatMsg col str   -> handleChatMsg col str >> return False
      _                 -> error "handleServerInput - unexpected Message"
    unless quit go

  handleGameMsg :: GameState -> ValidMoves -> String -> IO Bool
  handleGameMsg gs vm msg = do
    --  WHNF to avoid computing the board string inside withMVar
    let !boardStr = runRenderColor gs gc vm
        putBoardStrFunc = if isProxy then putColoredBoardHtml
                               else (putChar '\n' >>) . putColoredBoardTerm
    withMVar writeLock $ \_ -> do
      putBoardStrFunc boardStr
      flushStrLn msg
      flushStrLn $ case (winner gs, color $ currP gs) of
                   (Just c, _) -> msgGameEnd c
                   (Nothing, c) | c == myColor -> msgYourTurn
                                | otherwise    -> msgAwaitingTurn c
      return $ isJust $ winner gs
  handleChatMsg :: Color -> String -> IO ()
  handleChatMsg col msg = when isProxy $ putChatMessageJson col msg

handleUserInput :: Color -> Socket -> MVar () -> IO ()
handleUserInput c sock writeLock = go
 where
  go = do
    input <- getLine
    either
      (\err -> withMVar writeLock $ \_ -> putStrLn err)
      (`sendMsg` sock)
      $ parseMessage c input
    go

{-recvFromServer :: (Functor m, MonadIO m) => Socket -> m Message-}
{-recvFromServer = recvMsg-}

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
{-emptyInput :: IO ()-}
{-emptyInput = do-}
  {-inputExists <- hReady stdin-}
  {-when inputExists $ getLine >> emptyInput-}

