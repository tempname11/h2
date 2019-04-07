{-# LANGUAGE FlexibleContexts #-}
module Heroes.Platform where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Common
import Heroes.Image                                      (AnyImage)
import Heroes.SpriteMeta                                 (Palette)
import qualified Heroes.WND                                as WND
import qualified GLES                                      as GL
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Control.Concurrent                                (ThreadId)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

class Platform where
  -- XXX move to GFX?
  productionPrefix :: String
  staticSpriteExtension :: String
  --
  forkPreferred :: IO () -> IO ThreadId
  -- XXX GLESHelpers?
  createQuadArray :: IO GL.AnyArray
  generatePaletteArray :: Palette -> IO GL.UInt8Array
  loadGLSL :: String -> IO GL.GLString
  getGLContext :: WND.Window -> IO GL.Ctx
  -- XXX ImageHelpers?
  loadImage :: String -> IO (Either String AnyImage)
