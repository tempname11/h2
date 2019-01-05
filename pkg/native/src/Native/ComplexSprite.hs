module Native.ComplexSprite (
  loadCreature,
  loadSFX,
  destroy,
  createPalettedSurface,
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes.Essentials                                 (Essentials(..))
import Heroes.Platform
import Heroes.SpriteMeta
import Native
import Native.Platform
import qualified Heroes.FilePath                           as FilePath
import qualified Heroes.H3                                 as H3
import qualified Heroes.UI.Specials                        as Specials
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Codec.Picture                             as Juicy
import qualified Data.Vector.Storable                      as SV
import qualified Data.Vector.Storable.Mutable              as MSV
import qualified SDL
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

loadSFX :: Platform => Essentials -> SFX -> IO ComplexSprite
loadSFX (Essentials {..}) s = do
  let
    pngPath = FilePath.pngPathOf (H3.sDefName s)
    meta = sfxMeta s
  load meta pngPath

loadCreature :: Platform => Essentials -> Creature -> IO ComplexSprite
loadCreature (Essentials {..}) c = do
  let
    pngPath = FilePath.pngPathOf (H3.cDefName c)
    meta = creatureMeta c
  load meta pngPath

load :: Meta -> String -> IO ComplexSprite
load meta pngPath = do
  putStrLn $ "Loading... " <> pngPath
  result <- Juicy.readPng pngPath
  --
  Juicy.Image _ _ pixels <- case result of
    Left str -> raise str
    Right (Juicy.ImageY8 i) -> return i
    _ -> raise "Juicy image format mismatch."
  --
  mpixels <- SV.unsafeThaw pixels
  --
  (surface, palette) <- createPalettedSurface mpixels
    (meta ^. palette_) (meta ^. dimensions_)
  --
  return $ NativeComplexSprite {
    surface = surface,
    palette = palette,
    groups  = meta ^. groups_
  }

destroy :: ComplexSprite -> IO ()
destroy s = SDL.freeSurface (s ^. surface_)

--------------------------------------------------------------------------------

createPalettedSurface :: MSV.IOVector Word8
                      -> SV.Vector (V4 Word8)
                      -> V2 CInt
                      -> IO (SDL.Surface, SDL.Palette)
createPalettedSurface mpixels colors (V2 w h) = do
  surface <- SDL.createRGBSurfaceFrom mpixels (V2 w h) w SDL.Index8 -- XXX?
  format  <- SDL.surfaceFormat surface
  m       <- SDL.formatPalette format
  let palette = m
        & presumeJust "8-bit SDL surfaces should have a palette. \
        \ Reference: https://wiki.libsdl.org/SDL_CreateRGBSurfaceFrom"
  SDL.setPaletteColors palette colors 0
  SDL.setPaletteColors palette Specials.defaults 0
  return (surface, palette)
