module VConn where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Common
import qualified WSC
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Control.Concurrent                        as C
import qualified Reflex.Jumpstart                          as J
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

reconnectDelayUs :: Int
reconnectDelayUs = 1000 * 1000

data Conn u d = Conn {
  up :: u -> IO (),
  down :: Ev d
} deriving (Generic)

vconn ::
  (WSC.WSC, Binary u, Binary d) =>
  WSC.ConnectionOptions ->
  IO (Ev (Maybe (Conn u d)))
vconn opts = do
  (e, f) <- J.extern
  void $ C.forkIO $ fix $ \again -> do
    sendM <- C.newEmptyMVar
    -- TODO catch errors?
    WSC.connect opts $ \conn -> do
      (down, fd) <- J.extern
      let
        up = C.putMVar sendM
      --
      J.fire [f $ Just $ Conn { up, down }]
      void $ C.forkIO $ recvThread fd conn
      sendThread sendM conn
    J.fire [f Nothing]
    C.threadDelay reconnectDelayUs
    again
  return e

sendThread :: (WSC.WSC, Binary u) => C.MVar u -> WSC.Connection -> IO ()
sendThread sendM conn = fix $ \again -> do
  u <- C.takeMVar sendM
  WSC.sendEncoded conn u
  again
    
recvThread :: (WSC.WSC, Binary d) => (d -> J.F) -> WSC.Connection -> IO ()
recvThread f conn = fix $ \again -> do
  d <- WSC.recvDecoded conn
  J.fire [f d]
  again
