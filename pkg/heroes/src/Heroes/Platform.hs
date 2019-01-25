{-# LANGUAGE FlexibleContexts #-}
module Heroes.Platform where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Common
import GLES                                              (GLES)
import qualified GLES                                      as GL
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Control.Concurrent                                (ThreadId)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

class (GLES) => Platform where
  --
  -- XXX move to GFX?
  productionPrefix :: String
  staticSpriteExtension :: String
  --
  forkPreferred :: IO () -> IO ThreadId
  --
  type Chunk
  loadChunk :: String -> IO Chunk
  freeChunk :: Chunk -> IO ()
  --
  -- XXX move to GLHelpers?
  createQuadArray :: IO GL.AnyArray
  loadGLString :: String -> IO GL.GLString
