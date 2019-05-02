module Heroes.ActionQueue where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Utils.NBChan                                      (NBChan)
import qualified Utils.NBChan                              as NBChan
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Control.Concurrent                                (MVar)
import Control.Concurrent                                (newMVar)
import Control.Concurrent                                (putMVar)
import Control.Concurrent                                (takeMVar)
import Control.Concurrent                                (readMVar)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data ActionQueue a = ActionQueue {
  chan :: NBChan a,
  tokenM :: MVar Int
} deriving (Generic)

new :: IO (ActionQueue a)
new = do
  chan <- NBChan.new
  tokenM <- newMVar 0
  return (ActionQueue {..})

take1 :: ActionQueue a -> IO (Maybe a)
take1 (ActionQueue {..}) = NBChan.take1 chan

useDispatch :: ActionQueue a -> IO (a -> IO ())
useDispatch (ActionQueue {..}) = do
  oldToken <- takeMVar tokenM
  let newToken = oldToken + 1
  putMVar tokenM newToken
  return $ \x -> do
    token <- readMVar tokenM
    when (newToken == token) $ NBChan.put1 chan x
    
