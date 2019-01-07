{-# LANGUAGE FlexibleContexts #-}
module Heroes.Platform where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Common
import Heroes.SpriteMeta                                 (Meta)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Control.Concurrent                                (ThreadId)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

class Show ComplexSprite => Platform where
  --
  productionPrefix :: String
  staticSpriteExtension :: String
  forkPreferred :: IO () -> IO ThreadId
  --
  type InputProvider
  type Renderer
  type CursorResources
  type StaticSprite
  --
  type ComplexSprite
  loadComplexSprite :: Meta -> String -> IO ComplexSprite
  destroyComplexSprite :: ComplexSprite -> IO ()
  --
  type Chunk
  loadChunk :: String -> IO Chunk
  freeChunk :: Chunk -> IO ()
