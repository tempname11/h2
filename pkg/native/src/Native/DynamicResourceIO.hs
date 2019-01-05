module Native.DynamicResourceIO where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes.CreatureResource                           (CreatureResource(..))
import Heroes.SFXResource                                (SFXResource(..))
import Native
import qualified Stage.Links                               as L
import qualified Heroes.H3                                 as H3
import qualified Heroes.UI.Sound                           as Sound
import qualified Heroes.FilePath                           as FilePath
import qualified Native.ComplexSprite                      as ComplexSprite
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified SDL.Mixer                                 as Mix
import qualified Data.Map.Strict                           as M
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Deps = Deps {
  essentials :: L.Essentials
}

loadCreature :: Platform => Deps -> Creature -> IO CreatureResource
loadCreature (Deps {..}) c = do
  sprite <- ComplexSprite.loadCreature essentials c
  --
  let soundTypes = Sound.allTypes & if H3.shoots c
                                    then id
                                    else filter (/= Sound.Shot)
  --
  sounds <- fmap M.fromList $ for soundTypes $ \t -> do
    let path = FilePath.h3 <> "Sounds/" <> prefix <> suffix <> ".wav"
        prefix = H3.cSndName c
        suffix = Sound.suffix t
    putStrLn $ "Loading sound... " <> path
    (,) t <$> Mix.load path
  --
  return $ CreatureResource { sprite, sounds }

destroyCreature :: CreatureResource -> IO ()
destroyCreature c = do
  ComplexSprite.destroy (c ^. sprite_)
  mapM_ Mix.free (c ^. sounds_)

loadSFX :: Platform => Deps -> SFX -> IO SFXResource
loadSFX (Deps {..}) s = do
  sprite <- ComplexSprite.loadSFX essentials s
  sound <- do
    let path = FilePath.h3 <> "Sounds/" <> H3.sSndName s <> ".wav"
    putStrLn $ "Loading sound... " <> path
    Mix.load path
  --
  return $ SFXResource { sprite, sound }

destroySFX :: SFXResource -> IO ()
destroySFX c = do
  ComplexSprite.destroy (c ^. sprite_)
  Mix.free (c ^. sound_)
