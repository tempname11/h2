module Native.LobbyServer where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes.Protocol'Lobby
import Native
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Data.Binary                                       (decode)
import Data.Binary                                       (encode)
import qualified Network.WebSockets                        as WS
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

lobbyPort :: Int
lobbyPort = 10000

main' :: IO ()
main' = do
 WS.runServer "0.0.0.0" lobbyPort $ \pending -> do
  c <- WS.acceptRequest pending
  bs <- WS.receiveDataMessage c >>= \case
    WS.Text _ -> raise "DataMessage: Text!?"
    WS.Binary bs -> return bs
  let req = decode bs
  case req of
    Request'ListMatches -> do
      WS.sendDataMessage c $
        WS.Binary . encode $ Response'Matches
          [
            MatchInfo { name = "Transmittin'" }
          ]
