module Heroes.Subsystems.WND where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Prov = Prov {
  window :: Window
}

class WND where
  type Window
  with :: With Prov
