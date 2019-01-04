{-# OPTIONS_GHC -Wno-orphans #-}
module Native.Config where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Native
import Platform.Config
import Heroes.UI                                         (viewportSize)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified SDL
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
  
instance Config where
  productionPrefix = ".production-assets/"
  staticSpriteExtension = ".bmp"

windowConfig :: SDL.WindowConfig
windowConfig = SDL.defaultWindow
  { SDL.windowPosition = SDL.Absolute (P $ V2 100 100)
  , SDL.windowInitialSize = (<§>) viewportSize }

rendererConfig :: SDL.RendererConfig
rendererConfig = SDL.RendererConfig
  { SDL.rendererType = SDL.AcceleratedVSyncRenderer
  , SDL.rendererTargetTexture = False }

