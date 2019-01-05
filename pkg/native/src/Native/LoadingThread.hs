module Native.LoadingThread (
  with,
  Deps (..),
  Prov (..),
  LoadingChannels
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes.CreatureResource                           (CreatureResource)
import Heroes.Platform                                   (forkPreferred)
import Heroes.SFXResource                                (SFXResource)
import Native
import Native.DynamicResourceIO                          (Deps (..))
import Native.DynamicResourceIO                          (loadCreature)
import Native.DynamicResourceIO                          (loadSFX)
import Utils.NBChan                                      (NBChan)
import qualified Utils.NBChan                             as NBChan
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Control.Concurrent                                (threadDelay)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

type LoadingChannels = (
    NBChan (Either SFX Creature),
    NBChan (Either (SFX, SFXResource) (Creature, CreatureResource))
  )

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
loadingThread deps (wishChan, loadedChan) = do
  let load (Left s) = Left . (s,) <$> loadSFX deps s
      load (Right c) = Right . (c,) <$> loadCreature deps c
  fix $ \again -> do
    threadDelay _THREAD_DELAY_
    ws <- NBChan.drain wishChan
    for_ ws $ \w -> do
      print (w, "is wished to be loaded. So be it!" :: String)
      threadDelay 1000000
      r <- load w
      NBChan.trickle loadedChan r
    again
