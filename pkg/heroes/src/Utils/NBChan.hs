module Utils.NBChan (
  new,
  put,
  put1,
  take,
  take1,
  NBChan
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Common
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Control.Concurrent                                (MVar)
import Control.Concurrent                                (newMVar)
import Control.Concurrent                                (putMVar)
import Control.Concurrent                                (takeMVar)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data NBChan a = NBChan {
  mvar :: MVar [a]
}

new :: IO (NBChan a)
new = do
  mvar <- newMVar []
  return (NBChan {..})

put :: NBChan a -> [a] -> IO ()
put (NBChan {..}) xs = do
  ys <- takeMVar mvar
  putMVar mvar (ys <> xs)
  
put1 :: NBChan a -> a -> IO ()
put1 n = put n . (: [])

take1 :: NBChan a -> IO (Maybe a)
take1 (NBChan {..}) = do
  xs <- takeMVar mvar
  case xs of
    x : xs' -> do
      putMVar mvar xs'
      return (Just x)
    [] -> do
      putMVar mvar []
      return Nothing

take :: NBChan a -> IO [a]
take (NBChan {..}) = do
  xs <- takeMVar mvar
  putMVar mvar []
  return xs
