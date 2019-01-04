module Web.Stage.ChangeTheDamnCursor_ (
  with,
  Deps (..),
  In (..),
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Web
import qualified Stage.Links                               as L
import Platform.Config                                   (Config)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Deps = Deps { noDeps :: () }
data In = In {
  intent :: L.Intent
}

--------------------------------------------------------------------------------

with :: Config => Deps -> ((In -> IO ()) -> IO a) -> IO a
with _ next = next $ const $ return ()
