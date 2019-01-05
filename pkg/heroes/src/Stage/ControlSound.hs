module Stage.ControlSound where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Common
import Stage.Loading                                     (Loaded)
import Stage.Blackbox                                    (SoundCommands)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Deps = Deps { noDeps :: () }

data In = In {
  loaded :: Loaded,
  soundCommands :: SoundCommands
}

class ControlSound where
  with :: Deps -> ((In -> IO ()) -> IO a) -> IO a
