{-# OPTIONS_GHC -Wno-orphans #-}
module Native.Stage.Prerequisites () where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes.UI                                         (viewportSize)
import Native
import Stage.Prerequisites
import qualified Heroes.Essentials                         as Essentials
import qualified Heroes.FilePath                           as FilePath
import qualified Native.ResourceIO                         as Resource
import qualified Native.UI.Cursor                          as Cursor
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.ByteString                           as B
import qualified Data.Vector                               as V
import qualified SDL
import Data.String                                       (fromString)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

instance Prerequisites where
  with _ next = do
    putStrLn $ "Loading essentials..."
    buf <- B.readFile FilePath.essentialsBin1
    essentials <- case parseWith Essentials.getIt buf of
      Left str -> raise ("Meta.parse failure: " <> str)
      Right m -> return m
    --
    window <- SDL.createWindow (fromString "Fight!") windowConfig
    renderer <- SDL.createRenderer window (-1) rendererConfig
    --
    staticResources <- Resource.init renderer
    cursorResources <-
      V.concat <$> for Cursor.stuff (uncurry Resource.loadCursor)
    --
    let inputProvider = ()
    --
    result <- next $ Prov {..}
    --
    (mapM_ . mapM_) SDL.freeCursor cursorResources
    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    Resource.fini staticResources
    return result

windowConfig :: SDL.WindowConfig
windowConfig = SDL.defaultWindow
  { SDL.windowPosition = SDL.Absolute (P $ V2 100 100)
  , SDL.windowInitialSize = (<§>) viewportSize }

rendererConfig :: SDL.RendererConfig
rendererConfig = SDL.RendererConfig
  { SDL.rendererType = SDL.AcceleratedVSyncRenderer
  , SDL.rendererTargetTexture = False }
