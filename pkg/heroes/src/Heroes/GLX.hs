module Heroes.GLX where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Common
import qualified Heroes.WND                                as WND
import qualified GLES                                      as GL
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

-- XXX rename to GL'Utils?
class GLX where
  getGLContext :: WND.Window -> IO GL.Ctx
  loadGLSL :: String -> IO GL.GLString
