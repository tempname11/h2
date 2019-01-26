module Heroes.Image where -- XXX rename module?

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

class Capability'Image where
  type AnyImage
  width :: AnyImage -> IO Int
  height :: AnyImage -> IO Int
