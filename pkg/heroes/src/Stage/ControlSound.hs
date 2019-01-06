module Stage.ControlSound where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Common
import Stage.Loading                                     (Loaded)
import qualified Heroes.UI.Sound                           as Sound
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.Vector                               as V
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Deps = Deps { noDeps :: () }

data In = In {
  loaded :: Loaded,
  soundCommands :: V.Vector Sound.Command
}

class ControlSound where
  with :: Deps -> ((In -> IO ()) -> IO a) -> IO a
