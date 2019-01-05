module Utils.NBChan (
  NBChan,
  new,
  drain,
  pour,
  trickle,
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Prelude
import Control.Monad                                     (replicateM)
import Control.Concurrent                                (Chan)
import Control.Concurrent                                (newChan)
import Control.Concurrent                                (readChan)
import Control.Concurrent                                (writeChan)
import Control.Concurrent                                (writeList2Chan)
import Control.Concurrent                                (MVar)
import Control.Concurrent                                (newMVar)
import Control.Concurrent                                (modifyMVar)
import Control.Concurrent                                (modifyMVar_)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data NBChan a = NBChan (Chan a) (MVar Int)

new :: IO (NBChan a)
new = NBChan <$> newChan <*> newMVar 0

drain :: NBChan a -> IO [a]
drain (NBChan c m) = do
  -- (0, x): 0 is the new MVar value, x is the output
  available <- modifyMVar m (return . (0,))
  replicateM available $
    readChan c

pour :: NBChan a -> [a] -> IO ()
pour (NBChan c m) xs = do
  writeList2Chan c xs
  modifyMVar_ m (return . (+ length xs))

trickle :: NBChan a -> a -> IO ()
trickle (NBChan c m) x = do
  writeChan c x
  modifyMVar_ m (return . (+ 1))
