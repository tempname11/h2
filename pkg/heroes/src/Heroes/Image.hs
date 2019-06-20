module Heroes.Image where -- XXX rename module?

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Common
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

class Capability'Image where
  type AnyImage
  width :: AnyImage -> IO Int
  height :: AnyImage -> IO Int
