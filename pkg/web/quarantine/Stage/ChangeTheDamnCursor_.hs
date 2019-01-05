module Web.Stage.ChangeTheDamnCursor_ (
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
  intent :: L.Intent
}

--------------------------------------------------------------------------------

with :: Platform => Deps -> ((In -> IO ()) -> IO a) -> IO a
with _ next = next $ const $ return ()
