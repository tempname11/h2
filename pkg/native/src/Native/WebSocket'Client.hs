{-# OPTIONS_GHC -Wno-orphans #-}
module Native.WebSocket'Client where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Native
import WebSocket'Client
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.ByteString.Lazy                      as BL
import qualified Network.WebSockets                        as WS
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

instance Websocket'Client where
  type Connection = WS.Connection
  connect (ConnectionOptions {..}) handler = WS.runClient host port path handler
  send c bs = WS.sendDataMessage c (WS.Binary (BL.fromStrict bs))
  recv c = WS.receiveDataMessage c >>= \case
    WS.Text _ -> raise "WebSocket'Client: text message"
    WS.Binary bs -> return (BL.toStrict bs)
