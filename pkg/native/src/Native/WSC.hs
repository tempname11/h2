{-# OPTIONS_GHC -Wno-orphans #-}
module Native.WSC where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Native
import WSC
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.ByteString.Lazy                      as BL
import qualified Network.WebSockets                        as WS
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

instance WSC where
  type Connection = WS.Connection
  connect (ConnectionOptions {..}) handler = WS.runClient host port path handler
  send c bs = WS.sendDataMessage c (WS.Binary (BL.fromStrict bs))
  recv c = WS.receiveDataMessage c >>= \case
    WS.Text _ -> raise "WebSocket'Client: text message"
    WS.Binary bs -> return (BL.toStrict bs)
