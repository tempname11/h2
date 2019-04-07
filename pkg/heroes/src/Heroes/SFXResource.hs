module Heroes.SFXResource where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Heroes.Essentials                                 (Essentials(..))
import Heroes.Platform                                   (Platform)
import qualified Heroes.FilePath                           as FilePath
import qualified Heroes.H3                                 as H3
import qualified Heroes.GFX                                as GFX
import qualified Heroes.SND                                as SND
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data SFXResource = SFXResource {
  sprite :: GFX.ComplexSprite,
  chunk :: SND.Chunk
} deriving (Generic)

--------------------------------------------------------------------------------

load ::
  (GFX.GFX, SND.SND, Platform) =>
  GFX.Renderer ->
  Essentials ->
  SFX ->
  IO SFXResource
load r (Essentials {..}) s = do
  let
    pngPath = FilePath.pngPathOf (H3.sDefName s)
    meta = sfxMeta s
  sprite <- GFX.loadComplexSprite r meta pngPath
  chunk <- do
    let path = FilePath.prod <> "Sounds/" <> H3.sSndName s <> ".wav"
    putStrLn $ "Loading sound... " <> path
    SND.loadChunk path
  --
  return $ SFXResource { sprite, chunk }

destroy :: (GFX.GFX, SND.SND) => SFXResource -> IO ()
destroy c = do
  GFX.destroyComplexSprite (c ^. _sprite)
  SND.freeChunk (c ^. _chunk)
