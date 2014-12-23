module Quoridor.Cmdline.Network.Common
  ( ConnPlayer (..)
  , sendToSock
  , recvFromSock
  ) where

import           Control.Applicative   ((<$>))
import           Control.Exception     (ErrorCall (..), handle)
import           Control.Monad.State   (liftIO)
import           Control.Monad.State   (MonadIO)
import qualified Data.ByteString.Char8 as BC
import           Data.Maybe            (fromJust)
import           Text.Printf           (printf)

import           Network.Simple.TCP    (Socket, recv, send)
import           Numeric               (readHex, showHex)

import           Quoridor

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

recvFromSock :: (Read r, Functor m, MonadIO m) => Socket -> m (Maybe r)
recvFromSock sock = liftIO $ handle (\(ErrorCall _) -> return Nothing) $ do
  mHexSize <- recv sock 4
  let ((size,_):_) = readHex $ BC.unpack $ fromJust mHexSize
  mValue <- recv sock size
  return $ (read . BC.unpack) <$> mValue
