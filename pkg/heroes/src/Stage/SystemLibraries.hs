module Stage.SystemLibraries where
-- XXX deprecated

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Common
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Prov = Prov {
  noProv :: ()
}

class SystemLibraries where
  with :: (Prov -> IO a) -> IO a
