module Heroes.Platform where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Common
import Heroes.SpriteMeta                                 (Meta)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Control.Concurrent                                (ThreadId)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

class Platform where
  productionPrefix :: String
  staticSpriteExtension :: String
  type StaticSprite
  --
  type ComplexSprite
  loadComplexSprite :: Meta -> String -> IO ComplexSprite
  destroyComplexSprite :: ComplexSprite -> IO ()
  --
  type Chunk
  loadChunk :: String -> IO Chunk
  freeChunk :: Chunk -> IO ()
  --
  forkPreferred :: IO () -> IO ThreadId
