module Heroes.Root.MenuScreen.Client where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Heroes.Protocol'Lobby
import WSC
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Control.Concurrent                                (threadDelay)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

listMatches :: WSC => IO (Async [MatchInfo])
listMatches = async $ do
  let
    host = "localhost"
    port = 10000
    path = "/"
  threadDelay $ 1000 * 1000
  connect (ConnectionOptions {..}) $ \c -> do
    sendEncoded c (Request'ListMatches)
    Response'Matches ms <- recvDecoded c
    return ms
