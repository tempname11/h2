module Reflex.Spider.Addenum where

import Control.Monad
import Control.Monad.IO.Class
import Data.Unique
import Prelude
import Reflex.Spider.Internal
import System.Mem.Weak

-- This is very brittle; Reflex was not designed for this.
--
-- Nevertheless, it seems to function.
--
-- The side-effect is run at most once each frame,
-- when the original event is fired.
--
-- When a root event is created, subscribed to, and disposed of,
-- this does not (seemingly! maybe it depends on circumstances)
-- create a memory leak.
--
subscribeEffect :: HasSpiderTimeline x => Event x a -> (a -> EventM x b) -> EventM x (Event x b)
subscribeEffect e f = do
  let e' = push (\a -> Just <$> f a) e
  u <- liftIO $ newUnique
  let p _ = return (seq u ())
  --
  (s, _) <-
    subscribeAndRead e' $
      terminalSubscriber p
  --
  void $ liftIO $ mkWeak u s $ Just (unsubscribe s)
  return e'
