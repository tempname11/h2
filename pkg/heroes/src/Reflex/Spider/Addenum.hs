module Reflex.Spider.Addenum where

import Control.Monad.IO.Class
import Data.IORef
import Prelude
import Reflex.Spider.Internal
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.Weak

-- necessary?
weaks :: IORef [Weak (EventSubscription x)]
weaks = unsafePerformIO (newIORef [])

subscribeEffect :: HasSpiderTimeline x => Event x a -> (a -> EventM x b) -> EventM x (Event x b)
subscribeEffect e f = do
  let e' = push (\a -> Just <$> f a) e
  defer $ SomeMergeInit $ do
    (subscription, _) <-
      subscribeAndRead e'
        (terminalSubscriber $ \_ -> return ())
    liftIO $ do
      w <- mkWeak e subscription Nothing
      -- can delete from `weaks` here
      atomicModifyIORef' weaks (\ws -> (w : ws, ()))
    return ()
  return e'
