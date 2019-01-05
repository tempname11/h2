module Heroes.Platform where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Common
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

class Platform where
  productionPrefix :: String
  staticSpriteExtension :: String
  type StaticSprite
  type ComplexSprite
  type Chunk
