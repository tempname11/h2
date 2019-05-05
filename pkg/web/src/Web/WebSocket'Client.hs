{-# OPTIONS_GHC -Wno-orphans #-}
module Web.WebSocket'Client where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Web
import WebSocket'Client
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Control.Concurrent                                (MVar)
import Control.Concurrent                                (newEmptyMVar)
import Control.Concurrent                                (putMVar)
import Control.Concurrent                                (takeMVar)
import qualified Data.JSString                             as JSString
import qualified GHCJS.Buffer                              as Buffer
import qualified JavaScript.Web.MessageEvent               as MessageEvent
import qualified JavaScript.Web.WebSocket                  as WS
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Conn = Conn {
  webSocket :: WS.WebSocket,
  recvQueue :: MVar ByteString
} deriving (Generic)

instance Websocket'Client where
  type Connection = Conn
  connect (ConnectionOptions {..}) handler = do
    recvQueue <- newEmptyMVar
    webSocket <- WS.connect (WS.WebSocketRequest {
      url = JSString.pack (host <> show port <> path),
      protocols = [],
      onClose = Nothing,
      onMessage = Just $ \e -> case MessageEvent.getData e of
        MessageEvent.StringData _ -> raise "WebSocket'Client: StringData!?"
        MessageEvent.BlobData _ -> raise "WebSocket'Client: BlobData!?"
        MessageEvent.ArrayBufferData a -> do
          let b = Buffer.createFromArrayBuffer a
          putMVar recvQueue (Buffer.toByteString 0 Nothing b)
    })
    handler (Conn {..})
  --
  send (Conn {..}) bs = do
    let
      (b, _, _) = Buffer.fromByteString bs
      a = Buffer.getArrayBuffer b
    --
    WS.sendArrayBuffer a webSocket
  --
  recv (Conn {..}) = do
    result <- takeMVar recvQueue
    return result
