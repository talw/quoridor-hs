module Quoridor.Cmdline
where

import Quoridor.Cmdline.Render (runRender)
import Quoridor.Cmdline.Parse (parseTurn)
import Quoridor (makeTurn, checkAndSetWinner, Game, gameConfig, runGame,
  Color(..), Turn, GameState(..), Player(..), currP)
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import System.Environment (getArgs)
import System.IO
import Quoridor.Cmdline.Options (getSettings, Settings(..))

import Network.Simple.TCP
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust)
import Data.Hex (hex)
import Text.Printf (printf)
import Numeric (readHex)
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

data ConnPlayer = ConnPlayer {
  coplSock :: Socket,
  coplColor :: Color
}

sendToPlayer :: (Show s, MonadIO m) => s -> ConnPlayer -> m ()
sendToPlayer s cnp = do
    send (coplSock cnp) $ B.pack $
      printf "%04s" $ hex $ show $ B.length serialized
    send (coplSock cnp) serialized
  where serialized = B.pack $ show s

recvFromPlayer :: MonadIO m => ConnPlayer -> m String
recvFromPlayer cnp = do
  mHexSize <- recv (coplSock cnp) 4
  let ((size,_):_) = readHex $ B.unpack $ fromJust mHexSize
  mValue <- recv (coplSock cnp) size
  return $ B.unpack $ fromJust mValue

hostServer :: Game IO ()
hostServer = listen (Host "127.0.0.1") "33996" $ \(lstnSock, hostAddr) -> do
    let getPlayers 0 socks = return socks
        getPlayers n socks = accept lstnSock $
          \(connSock, rmtAddr) -> getPlayers (n-1) $ connSock : socks
    socks <- getPlayers 2 []
    gc <- ask
    let connPs = getConnPlayers socks
    mapM_ (\p -> sendToPlayer (gc, coplColor p) p) connPs
    playServer connPs
  where getConnPlayers socks = zipWith ConnPlayer socks colors
        colors = map toEnum [0..]

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
                    sendToCurrPlayer msg
                    execValidTurn
                  Right turn -> do
                    wasValid <- makeTurn turn
                    if wasValid
                      then return turn
                      else do
                        sendToCurrPlayer "last Turn was invalid"
                        execValidTurn
          turn <- execValidTurn
          play $ show currColor ++ ":" ++ show turn

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
