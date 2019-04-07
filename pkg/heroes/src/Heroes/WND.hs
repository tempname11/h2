module Heroes.WND where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Heroes.Aux                                        (Annotation)
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
