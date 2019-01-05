module Native.Utils where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Native
import qualified Heroes.UI.Specials                        as Specials
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.Vector.Storable                      as SV
import qualified Data.Vector.Storable.Mutable              as MSV
import qualified SDL
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

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
