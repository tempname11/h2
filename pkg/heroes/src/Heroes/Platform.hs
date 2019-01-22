{-# LANGUAGE FlexibleContexts #-}
module Heroes.Platform where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Common
import GLES                                              (GLES)
import Heroes.SpriteMeta                                 (Meta)
import qualified GLES                                      as GL
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Control.Concurrent                                (ThreadId)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

class (GLES) => Platform where
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
  loadComplexSprite :: Renderer -> Meta -> String -> IO ComplexSprite
  destroyComplexSprite :: ComplexSprite -> IO ()
  --
  type Chunk
  loadChunk :: String -> IO Chunk
  freeChunk :: Chunk -> IO ()
  --
  createQuadArray :: IO GL.AnyArray
  loadGLString :: String -> IO GL.GLString
