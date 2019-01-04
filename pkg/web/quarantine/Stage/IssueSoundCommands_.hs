module Web.Stage.IssueSoundCommands_ (
  with,
  Deps (..),
  In (..),
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Web

import qualified Stage.Links                               as L
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Deps = Deps { noDeps :: () }

data In = In {
  soundCommands :: L.SoundCommands
}

--------------------------------------------------------------------------------

with :: Deps -> ((In -> IO ()) -> IO a) -> IO a
with _ next = next $ const $ return ()
