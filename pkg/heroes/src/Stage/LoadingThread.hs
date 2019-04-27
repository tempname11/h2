module Stage.LoadingThread (
  with,
  Deps (..),
  Prov (..),
  LoadingChannels,
  LoadRequest (..),
  LoadResult (..)
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Heroes.CreatureResource                           (CreatureResource)
import Heroes.Essentials                                 (Essentials)
import Heroes.Platform                                   (Platform)
import Heroes.SFXResource                                (SFXResource)
import Utils.NBChan                                      (NBChan)
import qualified Heroes.CreatureResource                   as CreatureResource
import qualified Heroes.GFX                                as GFX
import qualified Heroes.SND                                as SND
import qualified Heroes.SFXResource                        as SFXResource
import qualified Utils.NBChan                              as NBChan
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data LoadResult
  = LoadResult'Creature Creature CreatureResource
  | LoadResult'SFX SFX SFXResource

data LoadRequest
  = LoadRequest'Creature Creature
  | LoadRequest'SFX SFX
  deriving (Eq, Ord, Show)

type LoadingChannels = (
    NBChan LoadRequest,
    NBChan LoadResult
  )

data Deps = Deps {
  renderer :: GFX.Renderer,
  essentials :: Essentials
}

data Prov = Prov {
  loadingChannels :: LoadingChannels,
  load :: IO ()
}

with :: (GFX.GFX, SND.SND, Platform) => Deps -> (Prov -> IO a) -> IO a
with deps next = do
  wishChan <- NBChan.new
  loadedChan <- NBChan.new
  let loadingChannels = (wishChan, loadedChan)
  let load = (loadingThread deps loadingChannels)
  next $ Prov {..}

loadingThread :: (GFX.GFX, SND.SND, Platform) => Deps -> LoadingChannels -> IO ()
loadingThread (Deps {..}) (wishChan, loadedChan) = do
  let
    load = \case
      LoadRequest'SFX s ->
        LoadResult'SFX s <$> SFXResource.load renderer essentials s
      LoadRequest'Creature c ->
        LoadResult'Creature c <$> CreatureResource.load renderer essentials c
  --
  ws <- NBChan.drain wishChan
  for_ ws $ \w -> do
    print (w, "is wished to be loaded. So be it!" :: String)
    r <- load w
    NBChan.trickle loadedChan r
