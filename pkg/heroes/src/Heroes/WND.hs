module Heroes.WND where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Common
import Heroes.Common
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data In = In {
  intent :: Maybe Annotation
}

data Prov = Prov {
  window :: Window,
  waitForVsync :: IO (),
  changeCursor :: Handler In
}

class WND where
  type Window
  with :: With Prov
