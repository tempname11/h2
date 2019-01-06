module Stage.ChangeCursor where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Heroes.Aux                                        (Annotation)
import Heroes.Platform                                   (Platform)
import qualified Heroes.Platform                           as Platform
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Deps = Deps { noDeps :: () }

data In = In {
  cursorResources :: Platform.CursorResources,
  intent :: Maybe Annotation
}

class ChangeCursor where
  with :: Platform => Deps -> ((In -> IO ()) -> IO a) -> IO a
