module Heroes.Root.MenuScreen.Client where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Heroes.Protocol'Lobby
import WSC
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Control.Concurrent                                (threadDelay)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

listMatches :: WSC => IO (Async [ID "Match"])
listMatches = async $ do
  let
    host = "localhost"
    port = 10000
    path = "/"
  threadDelay $ 1000 * 1000
  connect (ConnectionOptions {..}) $ \c -> do
    sendEncoded c (U'ListCreatedMatches (ID 0))
    D'CreatedMatchesAre _ ms <- recvDecoded c
    return ms
