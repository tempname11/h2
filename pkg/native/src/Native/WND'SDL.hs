{-# OPTIONS_GHC -Wno-orphans #-}
module Native.WND'SDL () where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes.Cursor                                     (cursorMeta)
import Heroes.WND
import Heroes.UI                                         (viewportSize)
import Native
import Native.Platform ()
import qualified Heroes.Cursor                             as Cursor
import qualified Heroes.FilePath                           as FilePath
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Data.String                                       (fromString)
import SDL                                               (($=))
import qualified Codec.Picture                             as Juicy
import qualified Data.Map.Strict                           as M
import qualified Data.Vector.Storable                      as SV
import qualified SDL
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

type Cursors = Cursor.Type -> SDL.Cursor

instance WND where
  type Window = SDL.Window
  with next = do
    SDL.initialize [
        SDL.InitAudio,
        SDL.InitVideo, 
        SDL.InitEvents
      ]
    window <- SDL.createWindow (fromString "Fight!") windowConfig
    cursors <- loadCursors
    --
    let
      changeCursor (In {..}) = SDL.activeCursor $= cursors (Cursor.fromIntent intent)
      waitForVsync = SDL.glSwapWindow window -- XXX
    --
    next $ Prov {..}
    destroyCursors cursors
    SDL.destroyWindow window
    SDL.quit

windowConfig :: SDL.WindowConfig
windowConfig =
  SDL.defaultWindow
  {
    SDL.windowPosition = SDL.Absolute (P $ V2 100 100),
    SDL.windowInitialSize = (<§>) viewportSize
  }

loadCursors :: IO Cursors
loadCursors = do
  m <- for (M.fromList (genum <&> \t -> (t, t))) $ \t -> do
    let (fileName, hotspot) = cursorMeta t
    result <- Juicy.readPng (FilePath.cursorPathOf fileName)
    image <-
      case result of
        Left str -> raise str
        Right i -> return (Juicy.convertRGBA8 i)
    --
    let
      w = (§) $ Juicy.imageWidth image
      h = (§) $ Juicy.imageHeight image
    --
    pixels <- SV.unsafeThaw (Juicy.imageData image)
    surface <- SDL.createRGBSurfaceFrom pixels (V2 w h) (w * 4) SDL.ABGR8888
    SDL.surfaceColorKey surface $= Just (V4 0 255 255 255)
    cursor <- SDL.createColorCursor surface (P hotspot)
    SDL.freeSurface surface
    return cursor
  return $
    \t -> M.lookup t m &
      presumeJust "the map should have all cursor types \
      \ since it was created using `genum`."
    
destroyCursors :: Cursors -> IO ()
destroyCursors cs = for_ genum $ \t -> SDL.freeCursor (cs t)
