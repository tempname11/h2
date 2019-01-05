module Stage.LoadingThread ( -- XXX rename
  with,
  Deps (..),
  Prov (..),
  LoadingChannels
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Heroes.CreatureResource                           (CreatureResource)
import Heroes.Essentials                                 (Essentials)
import Heroes.Platform                                   (Platform)
import Heroes.Platform                                   (forkPreferred)
import Heroes.SFXResource                                (SFXResource)
import Utils.NBChan                                      (NBChan)
import qualified Heroes.CreatureResource                  as CreatureResource
import qualified Heroes.SFXResource                       as SFXResource
import qualified Utils.NBChan                             as NBChan
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Control.Concurrent                                (threadDelay)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

type LoadingChannels = (
    NBChan (Either SFX Creature),
    NBChan (Either (SFX, SFXResource) (Creature, CreatureResource))
  )

data Deps = Deps {
  essentials :: Essentials
}

data Prov = Prov {
  loadingChannels :: LoadingChannels
}

_THREAD_DELAY_ :: Int
_THREAD_DELAY_ = 5000 -- microseconds!

with :: Platform => Deps -> (Prov -> IO a) -> IO a
with deps next = do
  wishChan <- NBChan.new
  loadedChan <- NBChan.new
  let loadingChannels = (wishChan, loadedChan)
  void $ forkPreferred (loadingThread deps loadingChannels)
  next (Prov {..})

loadingThread :: Platform => Deps -> LoadingChannels -> IO ()
loadingThread (Deps {..}) (wishChan, loadedChan) = do
  let load (Left s) = Left . (s,) <$> SFXResource.load essentials s
      load (Right c) = Right . (c,) <$> CreatureResource.load essentials c
  fix $ \again -> do
    threadDelay _THREAD_DELAY_
    ws <- NBChan.drain wishChan
    for_ ws $ \w -> do
      print (w, "is wished to be loaded. So be it!" :: String)
      threadDelay 1000000
      r <- load w
      NBChan.trickle loadedChan r
    again
