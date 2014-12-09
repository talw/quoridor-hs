module Quoridor.Cmdline
where

import Quoridor.Cmdline.Render (runRender)
import Quoridor.Cmdline.Parse (parseTurn)
import Quoridor (makeTurn, checkAndSetWinner, Game, gameConfig, runGame,
  Color(..), Turn, GameState(..), Player(..), currP, GameConfig)
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import System.Environment (getArgs)
import System.IO
import Quoridor.Cmdline.Options (getOptions, Options(..), ExecMode(..))

import Network.Simple.TCP
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust)
import Text.Printf (printf)
import Numeric (readHex, showHex)
import Data.List (find)
import Control.Applicative ((<$>))

--TODO when beginning a playserver game,
--have to wrap the runGame with withSocketsDo
cmdlineMain :: IO ()
cmdlineMain = do
  args <- getArgs
  settings <- getSettings args
  void $ runGame (play stdin) $
    gameConfig (gatesPerPlayer settings) (boardSize settings)

{-runGameFromScript :: IO ()-}
{-runGameFromScript = do-}
  {-handle <- openFile "moves" ReadMode-}
  {-void $ runGame (play handle) defaultGameConfig-}

{-playLocalGame :: IO ()-}
{-playLocalGame = void $ runGame (play stdin) defaultGameConfig-}



-- Server and Client

data ConnPlayer = ConnPlayer {
  coplSock :: Socket,
  coplColor :: Color
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

sendToPlayer :: (Show s, MonadIO m) => s -> ConnPlayer -> m ()
sendToPlayer s cnp = sendToSock s $ coplSock cnp

recvFromPlayer :: MonadIO m => ConnPlayer -> m String
recvFromPlayer cnp = recvFromSock $ coplSock cnp

hostServer :: Game IO ()
hostServer = listen (Host "127.0.0.1") "33996" $ \(lstnSock, hostAddr) -> do
    let getPlayers n socks | n > 0 = accept lstnSock $
          \(connSock, rmtAddr) -> getPlayers (n-1) $ connSock : socks
        getPlayers 0 socks = do
            gc <- ask
            let colors = map toEnum [0..]
                getConnPlayers socks = zipWith ConnPlayer socks colors
                connPs = getConnPlayers socks
            mapM_ (\p -> sendToPlayer (gc, coplColor p) p) connPs
            playServer connPs
    getPlayers 2 []

playServer :: [ConnPlayer] -> Game IO ()
playServer connPs = play "Good luck!"
  where
    play msg = do
      gs <- get
      mapM_ (sendToPlayer (gs,msg)) connPs
      case winner gs of
        Just _ -> return ()
        Nothing -> do
          let currColor = color $ currP gs
              currConnP = fromJust $ find ((currColor ==) . coplColor) connPs
              sendToCurrPlayer x = sendToPlayer x currConnP
              execValidTurn = do
                strTurn <- recvFromPlayer currConnP
                let eTurn = parseTurn strTurn
                case eTurn of
                  Left msg -> do
                    sendToCurrPlayer (gs,msg)
                    execValidTurn
                  Right turn -> do
                    wasValid <- makeTurn turn
                    if wasValid
                      then return turn
                      else do
                        sendToCurrPlayer (gs,"last Turn was invalid")
                        execValidTurn
          turn <- execValidTurn
          play $ show currColor ++ ":" ++ show turn



-- Client

connectClient :: IO ()
connectClient = connect "127.0.0.1" "33996" $ \(connSock, rmtAddr) -> do
  (gc, c) <- recvFromSock connSock
  playClient connSock gc c

playClient :: Socket -> GameConfig -> Color -> IO ()
playClient connSock gc myColor = play
  where
    play = do
      (gs, msg) <- recvFromSock connSock
      putStr $ runRender gs gc
      putStrLn msg
      case winner gs of
        Just c -> liftIO $ putStrLn $ show c ++ " won!"
        Nothing -> do
          let currPC = color $ currP gs
          if currPC /= myColor
            then do
              putStrLn $ "Waiting for " ++ show currPC ++ " to make a move."
              play
            else do
              strTurn <- liftIO getLine
              sendToSock strTurn connSock
              play

play :: Handle -> Game IO ()
play h = do
  gc <- ask
  let go showBoard msg = do
        gs <- get
        liftIO $ putStr $ runRender gs gc
        liftIO $ putStrLn msg
        case winner gs of
          Just c -> liftIO $ putStrLn $ show c ++ " won!"
          Nothing -> do
            strTurn <- liftIO $ hGetLine h
            let eTurn = parseTurn strTurn
            case eTurn of
              Left msg -> go False msg
              Right turn -> do
                wasValid <- makeTurn turn
                go True $ "last Turn was "
                            ++ (if not wasValid then "in" else "")
                            ++ "valid"
  go True "Good luck!"
