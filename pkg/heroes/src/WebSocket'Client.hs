module WebSocket'Client where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Common
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data ConnectionOptions = ConnectionOptions {
  host :: String,
  port :: Int,
  path :: String
} deriving (Generic)

class Websocket'Client where
  type Connection
  connect :: ConnectionOptions -> (Connection -> IO a) -> IO a
  send :: Connection -> ByteString -> IO ()
  recv :: Connection -> IO ByteString
