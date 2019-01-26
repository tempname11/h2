module Heroes.WND where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Heroes.Aux                                        (Annotation)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data In = In {
  cursorResources :: CursorResources,
  intent :: Maybe Annotation
}

data Prov = Prov {
  window :: Window,
  cursorResources :: CursorResources,
  waitForVsync :: IO (),
  changeCursor :: Handler In
}

class WND where
  type Window
  type CursorResources
  with :: With Prov
