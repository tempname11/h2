module Stage.Prerequisites where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Heroes.Essentials                                 (Essentials)
import Heroes.Platform                                   (Platform)
import qualified Heroes.Platform                           as Platform
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Deps = Deps { noDeps :: () }

data Prov = Prov {
  inputProvider :: Platform.InputProvider,
  cursorResources :: Platform.CursorResources,
  essentials :: Essentials
}

class Prerequisites where
  with :: Platform => Deps -> (Prov -> IO a) -> IO a
