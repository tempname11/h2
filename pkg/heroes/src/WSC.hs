module WSC where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Common
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Data.Binary                                       (Binary)
import Data.Binary                                       (decode)
import Data.Binary                                       (encode)
import Data.ByteString.Lazy                              (toStrict)
import Data.ByteString.Lazy                              (fromStrict)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data ConnectionOptions = ConnectionOptions {
  host :: String,
  port :: Int,
  path :: String
} deriving (Generic)

class WSC where
  type Connection
  connect :: ConnectionOptions -> (Connection -> IO a) -> IO a
  send :: Connection -> ByteString -> IO ()
  recv :: Connection -> IO ByteString

-- TODO?: avoid lazy <-> strict conversion
sendEncoded :: (WSC, Binary a) => Connection -> a -> IO ()
sendEncoded c = send c . toStrict . encode

-- TODO?: handle failure
recvDecoded :: (WSC, Binary a) => Connection -> IO a
recvDecoded c = decode . fromStrict <$> recv c
