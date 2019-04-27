module Utils.NBChan (
  NBChan,
  new,
  drain,
  pour,
  trickle,
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Common
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

-- XXX remove this file, it's now only a hack until
-- LoadingThread gets properly fixed

data NBChan a = XXX (IORef [a])

new :: IO (NBChan a)
new = XXX <$> newIORef []

drain :: NBChan a -> IO [a]
drain (XXX r) = do
  xs <- readIORef r
  writeIORef r []
  return xs

pour :: NBChan a -> [a] -> IO ()
pour (XXX r) xs = do
  ys <- readIORef r
  writeIORef r (ys <> xs)

trickle :: NBChan a -> a -> IO ()
trickle r x = pour r [x]
