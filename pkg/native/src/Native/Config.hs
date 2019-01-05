module Native.Config where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Native
import Heroes.UI                                         (viewportSize)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified SDL
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
  
windowConfig :: SDL.WindowConfig
windowConfig = SDL.defaultWindow
  { SDL.windowPosition = SDL.Absolute (P $ V2 100 100)
  , SDL.windowInitialSize = (<§>) viewportSize }

rendererConfig :: SDL.RendererConfig
rendererConfig = SDL.RendererConfig
  { SDL.rendererType = SDL.AcceleratedVSyncRenderer
  , SDL.rendererTargetTexture = False }

