module Heroes.SFXResource where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Heroes.Essentials                                 (Essentials(..))
import Heroes.Platform                                   (Platform)
import qualified Heroes.FilePath                           as FilePath
import qualified Heroes.H3                                 as H3
import qualified Heroes.Platform                           as Platform
import qualified Heroes.GFX                                as GFX
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data SFXResource = SFXResource {
  sprite :: GFX.ComplexSprite,
  sound :: Platform.Chunk
} deriving (Generic)

--------------------------------------------------------------------------------

load ::
  (GFX.GFX, Platform) =>
  GFX.Renderer ->
  Essentials ->
  SFX ->
  IO SFXResource
load r (Essentials {..}) s = do
  let
    pngPath = FilePath.pngPathOf (H3.sDefName s)
    meta = sfxMeta s
  sprite <- GFX.loadComplexSprite r meta pngPath
  sound <- do
    let path = FilePath.prod <> "Sounds/" <> H3.sSndName s <> ".wav"
    putStrLn $ "Loading sound... " <> path
    Platform.loadChunk path
  --
  return $ SFXResource { sprite, sound }

destroy :: (GFX.GFX, Platform) => SFXResource -> IO ()
destroy c = do
  GFX.destroyComplexSprite (c ^. _sprite)
  Platform.freeChunk (c ^. _sound)
