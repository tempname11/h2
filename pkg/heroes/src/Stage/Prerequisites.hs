module Stage.Prerequisites where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Heroes.Essentials                                 (Essentials)
import Heroes.Platform                                   (Platform)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Deps = Deps { noDeps :: () }

data Prov = Prov {
  essentials :: Essentials
}

class Prerequisites where
  with :: Platform => Deps -> (Prov -> IO a) -> IO a
