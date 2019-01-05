{-# LANGUAGE TemplateHaskell #-}
module Heroes.SFXResource where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Heroes.Essentials                                 (Essentials(..))
import Heroes.Platform                                   (Platform)
import qualified Heroes.FilePath                           as FilePath
import qualified Heroes.H3                                 as H3
import qualified Heroes.Platform                           as Platform
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data SFXResource = SFXResource {
  sprite :: Platform.ComplexSprite,
  sound :: Platform.Chunk
}

makeShorthands ''SFXResource

--------------------------------------------------------------------------------

load :: Platform => Essentials -> SFX -> IO SFXResource
load (Essentials {..}) s = do
  let
    pngPath = FilePath.pngPathOf (H3.sDefName s)
    meta = sfxMeta s
  sprite <- Platform.loadComplexSprite meta pngPath
  sound <- do
    let path = FilePath.h3 <> "Sounds/" <> H3.sSndName s <> ".wav"
    putStrLn $ "Loading sound... " <> path
    Platform.loadChunk path
  --
  return $ SFXResource { sprite, sound }

destroy :: Platform => SFXResource -> IO ()
destroy c = do
  Platform.destroyComplexSprite (c ^. sprite_)
  Platform.freeChunk (c ^. sound_)
