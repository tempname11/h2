{-# LANGUAGE TemplateHaskell #-}
module Native.Prerequisites (
  with,
  Cursors,
  Prov (..),
  Deps (..),
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes.StaticResources                            (StaticResources)
import Native
import qualified Heroes.Essentials                         as Essentials
import qualified Heroes.FilePath                           as FilePath
import qualified Native.Config                             as Config
import qualified Native.ResourceIO                         as Resource
import qualified Native.UI.Cursor                          as Cursor
import qualified Stage.Links                               as L
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.ByteString                           as B
import qualified Data.Vector                               as V
import qualified SDL
import Data.String                                       (fromString)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Deps = Deps { noDeps :: () }

type Cursors = V.Vector (V.Vector SDL.Cursor)

data Prov = Prov {
  renderer :: SDL.Renderer,
  cursors :: Cursors,
  essentials :: L.Essentials,
  staticResources :: StaticResources
}

with :: Platform => Deps -> (Prov -> IO a) -> IO a
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
  cursors <- V.concat <$> for Cursor.stuff (uncurry Resource.loadCursor)
  --
  result <- next $ Prov {..}
  --
  (mapM_ . mapM_) SDL.freeCursor cursors
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  Resource.fini staticResources

  return result
