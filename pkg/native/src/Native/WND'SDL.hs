{-# OPTIONS_GHC -Wno-orphans #-}
module Native.WND'SDL () where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes.Subsystems.WND
import Heroes.UI                                         (viewportSize)
import Native
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Data.String                                       (fromString)
import qualified SDL
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

instance WND where
  type Window = SDL.Window
  with next = do
    SDL.initialize [
        SDL.InitAudio,
        SDL.InitVideo, 
        SDL.InitEvents
      ]
    window <- SDL.createWindow (fromString "Fight!") windowConfig
    next (Prov {..})
    SDL.destroyWindow window
    SDL.quit

windowConfig :: SDL.WindowConfig
windowConfig =
  SDL.defaultWindow
  {
    SDL.windowPosition = SDL.Absolute (P $ V2 100 100),
    SDL.windowInitialSize = (<§>) viewportSize
  }

