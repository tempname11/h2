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

data VConn u d = VConn {
  up :: C.MVar u,
  down :: C.MVar d
} deriving (Generic)

vconn ::
  (Binary u, Binary d) =>
  WSC.ConnectionOptions ->
  IO (E (Maybe (VConn u d)))
vconn opts = do
  (e, f) <- J.extern
  forkIO $ fix $ \again -> do
    u <- C.newEmptyMVar
    d <- C.newEmptyMVar
    WSC.connect opts $ \conn -> do
      J.fire [f $ Just $ VConn { up, down }]
      C.forkIO $ recvThread recvM conn
      sendThread sendM conn
    J.fire [f Nothing]
    C.threadDelay reconnectDelayUs
    again
  return e

sendThread :: (Binary u) => C.MVar u -> WSC.Connection -> IO ()
sendThread sendM conn = fix $ \again -> do
  u <- C.takeMVar
  WSC.sendEncoded u
  again
    
recvThread :: (Binary d) => C.MVar d -> WSC.Connection -> IO ()
recvThread conn = fix $ \again -> do
  d <- WSC.recvDecoded conn
  C.putMVar recvM d
  again

vctx :: E (Maybe b) -> (a -> E (Maybe b)) -> E (Maybe b)
vctx f ea = do
  ea >>= \case
    Nothing -> return Nothing
    Just a -> f a

{-
ev <- vconn opts
vctx ev $ \VConn ({ up, down }) -> do
  up SomeCmd
  down >>= case
    WhatWeNeed -> ...
    _ -> empty
-}

