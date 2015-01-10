{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Quoridor.Cmdline.Network.Common
  ( ConnPlayer (..)
  , Message (..)
  , sendMsg
  , recvMsg
  , isAliveSock
  ) where

import           Control.Exception      (Exception, SomeException, catch,
                                         handle, throw)
import           Control.Monad.State    (MonadIO, liftIO, replicateM_)
import qualified Data.ByteString.Char8  as BC
import           Data.Maybe             (fromJust)
import           Data.Typeable          (Typeable)
import           Text.Printf            (printf)

import           Control.Concurrent.STM (TChan)
import           Network.Simple.TCP     (Socket, recv, send)
import           Numeric                (readHex, showHex)

import           Quoridor

data ConnPlayer = ConnPlayer
  { coplChan  :: TChan Message
  , coplColor :: Color
  }

data NetworkException = NetworkException
  deriving (Show, Typeable)
instance Exception NetworkException

data Message = FstGameMsg GameConfig Color
             | GameMsg GameState ValidMoves String
             | ChatMsg Color String
             | TurnMsg Turn
             | WaitMsg String
  deriving (Read, Show)

-- | Type-safer than sendToSock/recvFromSock duo
sendMsg :: (MonadIO m) => Message -> Socket -> m ()
sendMsg = sendToSock

sendToSock :: (Show s, MonadIO m) => s -> Socket -> m ()
sendToSock s sock = do
  send sock $ BC.pack $
    printf "%04s" $ showHex (BC.length serialized) ""
  send sock serialized
 where serialized = BC.pack $ show s

isAliveSock :: (MonadIO m) => Socket -> m Bool
isAliveSock sock = liftIO $
  do replicateM_ 2 $ (send sock . BC.pack) "0000"
     return True
  `catch`
  \(_ :: SomeException) -> return False

-- | Type-safer than sendToSock/recvFromSock duo
recvMsg :: (MonadIO m, Functor m) => Socket -> m Message
recvMsg = recvFromSock
{-recvMsg = (fromJust <$>) . recvFromSock-}

recvFromSock :: (Read r, Functor m, MonadIO m) => Socket -> m r
recvFromSock sock = liftIO $ handle handler $ do
  mHexSize <- recv sock 4
  let ((size,_):_) = readHex $ BC.unpack $ fromJust mHexSize
  if size == 0
    -- To handle size 0 messages which are just isAlive check
    then recvFromSock sock
    else do
      mValue <- recv sock size
      return $ read $ BC.unpack $ fromJust mValue
 where
  handler (_ :: SomeException) = throw NetworkException
