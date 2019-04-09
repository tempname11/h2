module Heroes.GLX where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Heroes.SpriteMeta                                 (Palette)
import qualified Heroes.WND                                as WND
import qualified GLES                                      as GL
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

-- XXX rename to GL'Utils?
class GLX where
  getGLContext :: WND.Window -> IO GL.Ctx
  createQuadArray :: IO GL.AnyArray
  generatePaletteArray :: Palette -> IO GL.UInt8Array
  loadGLSL :: String -> IO GL.GLString
