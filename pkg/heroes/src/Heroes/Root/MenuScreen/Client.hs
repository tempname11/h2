module Heroes.Root.MenuScreen.Client where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Heroes.Protocol'Lobby
--import WSC
import VConn
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
--import Control.Concurrent                                (threadDelay)
import qualified Reflex.Jumpstart                          as J
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

matches :: WSC => Ev (Maybe (Conn U D)) -> Ev (Maybe [ID "Match"])
matches v = do
  v >>= \case
    Nothing -> return Nothing
    Just _ -> do
      J.affect $ do
        undefined
      

{-
matches = async $ do
  let
    host = "localhost"
    port = 10000
    path = "/"
  threadDelay $ 1000 * 1000
  connect (ConnectionOptions {..}) $ \c -> do
    sendEncoded c (U'ListCreatedMatches (ID 0))
    D'CreatedMatchesAre _ ms <- recvDecoded c
    return ms
-}
