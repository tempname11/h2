module Stage.ChangeCursor where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Heroes.Platform                                   (Platform)
import qualified Heroes.Platform                           as Platform
import qualified Stage.Links                               as L
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Deps = Deps { noDeps :: () }

data In = In {
  intent :: L.Intent,
  cursorResources :: Platform.CursorResources
}

class ChangeCursor where
  with :: Platform => Deps -> ((In -> IO ()) -> IO a) -> IO a
