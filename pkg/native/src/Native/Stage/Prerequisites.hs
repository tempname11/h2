{-# OPTIONS_GHC -Wno-orphans #-}
module Native.Stage.Prerequisites () where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Native
import Stage.Prerequisites
import qualified Heroes.Essentials                         as Essentials
import qualified Heroes.FilePath                           as FilePath
import qualified Native.Config                             as Config
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
    window <- SDL.createWindow (fromString "Fight!") Config.windowConfig
    renderer <- SDL.createRenderer window (-1) Config.rendererConfig
    --
    staticResources <- Resource.init renderer
    cursorResources <-
      V.concat <$> for Cursor.stuff (uncurry Resource.loadCursor)
    --
    result <- next $ Prov {..}
    --
    (mapM_ . mapM_) SDL.freeCursor cursorResources
    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    Resource.fini staticResources
    return result
