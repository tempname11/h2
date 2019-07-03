module VConn where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Common
import qualified WSC
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Control.Concurrent                        as C
import qualified Reflex.Jumpstart                          as J
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

type Ev = J.E
type Be = J.B

reconnectDelayUs :: Int
reconnectDelayUs = 1000 * 1000

data VConn u d = VConn {
  up :: u -> Ev (),
  down :: Ev d
} deriving (Generic)

vconn ::
  (WSC.WSC, Binary u, Binary d) =>
  WSC.ConnectionOptions ->
  IO (Be (Maybe (VConn u d)))
vconn opts = do
  (e, f) <- J.extern
  b <- J.hold Nothing e
  void $ C.forkIO $ fix $ \again -> do
    sendM <- C.newEmptyMVar
    WSC.connect opts $ \conn -> do
      (down, fd) <- J.extern
      let
        up = J.affect . C.putMVar sendM
      --
      J.fire [f $ Just $ VConn { up, down }]
      void $ C.forkIO $ recvThread fd conn
      sendThread sendM conn
    J.fire [f Nothing]
    C.threadDelay reconnectDelayUs
    again
  return b

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

vctx :: Be (Maybe a) -> (a -> IO (Be (Maybe b))) -> Be (Maybe b)
vctx ba f = do
  ba >>= \case
    Nothing -> return Nothing
    Just a -> join $ J.affect $ f a

{-
bv <- vconn opts
vctx bv $ \VConn ({ up, down }) -> do
  up SomeCmd
  down >>= case
    WhatWeNeed -> ...
    _ -> empty
-}
