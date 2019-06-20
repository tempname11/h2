module Heroes.Root.MenuScreen.Client where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Heroes.Protocol'Lobby
import WSC
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

listMatches :: WSC => IO (Async [MatchInfo])
listMatches = async $ do
  let
    host = "localhost"
    port = 10000
    path = "/"
  print (101 :: Int)
  connect (ConnectionOptions {..}) $ \c -> do
    print (102 :: Int)
    sendEncoded c (Request'ListMatches)
    print (103 :: Int)
    Response'Matches ms <- recvDecoded c
    print (104 :: Int)
    return ms
  
