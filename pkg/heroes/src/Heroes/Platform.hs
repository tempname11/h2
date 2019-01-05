module Heroes.Platform where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Common
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Control.Concurrent                                (ThreadId)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

class Platform where
  productionPrefix :: String
  staticSpriteExtension :: String
  type StaticSprite
  type ComplexSprite
  type Chunk
  forkPreferred :: IO () -> IO ThreadId
