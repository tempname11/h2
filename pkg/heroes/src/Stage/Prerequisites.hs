module Stage.Prerequisites where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Heroes.Essentials                                 (Essentials)
import Heroes.Platform                                   (Platform)
import Heroes.StaticResources                            (StaticResources)
import qualified Heroes.Platform                           as Platform
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Deps = Deps { noDeps :: () }

data Prov = Prov {
  inputProvider :: Platform.InputProvider,
  renderer :: Platform.Renderer,
  cursorResources :: Platform.CursorResources,
  essentials :: Essentials,
  staticResources :: StaticResources
}

class Prerequisites where
  with :: Platform => Deps -> (Prov -> IO a) -> IO a
