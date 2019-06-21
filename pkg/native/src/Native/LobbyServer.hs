module Native.LobbyServer where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes.Protocol'Lobby
import Native
import qualified Battle.Example                            as Example
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Data.Binary                                       (decode)
import Data.Binary                                       (encode)
import qualified Data.Map.Strict                           as M
import qualified Network.WebSockets                        as WS
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

lobbyPort :: Int
lobbyPort = 10000

newGen :: (Int -> a) -> IO (IO a)
newGen f = do
  ref <- newIORef 0
  return $ do
    i <- readIORef ref
    writeIORef ref (i + 1)
    return $ f (i + 1)

send :: WS.Connection -> D -> IO ()
send c' d = WS.sendDataMessage c' $ WS.Binary . encode $ d

data Created = Created {
  m'ID :: ID "Match",
  creator'ID :: ID "Client",
  createRequest'ID :: ID "Request"
} deriving (Generic)

main' :: IO ()
main' = do
  connByID'R <- newIORef empty -- TODO weak map
  createdByID'R <- newIORef empty
  genMatchID <- newGen (ID @ "Match")
  genClientID <- newGen (ID @ "Client")
  WS.runServer "0.0.0.0" lobbyPort $ \pending -> do
    c <- WS.acceptRequest pending
    c'ID <- genClientID
    modifyIORef connByID'R $ M.insert c'ID c
    bs <- WS.receiveDataMessage c >>= \case
      WS.Text _ -> raise "DataMessage: Text!?"
      WS.Binary bs -> return bs
    --
    case decode bs of
      U'ListCreatedMatches { r'ID } -> do
        cm <- readIORef createdByID'R
        send c $ D'CreatedMatchesAre {
          r'ID,
          matches = M.keys cm
        }
      --
      U'CreateMatch { r'ID } -> do
        m'ID <- genMatchID
        modifyIORef createdByID'R $
          M.insert m'ID $
            Created {
              m'ID,
              creator'ID = c'ID,
              createRequest'ID = r'ID
            }
        send c $ D'MatchWasCreated { r'ID, m'ID }
      --
      U'JoinMatch { r'ID, m'ID } -> do
        let
          noSuchMatch = send c $ D'NoSuchMatch { r'ID }
        --
        M.lookup m'ID <$> readIORef createdByID'R >>= \case
          Nothing -> noSuchMatch
          Just (Created { creator'ID, createRequest'ID }) ->
            M.lookup creator'ID <$> readIORef connByID'R >>= \case
              Nothing -> noSuchMatch
              Just creatorC -> do
                let
                  setup = fst Example.one
                  initialBattle = snd Example.one
                --
                send creatorC $ D'MatchWasStarted {
                  team = Example.red,
                  setup,
                  initialBattle,
                  m'ID,
                  r'ID = createRequest'ID
                }
                --
                send c $ D'MatchWasStarted {
                  team = Example.blue,
                  setup,
                  initialBattle,
                  m'ID,
                  r'ID
                }
