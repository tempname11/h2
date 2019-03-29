module Native.GLES'Utils (makeContext) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Foreign
import Foreign.C.String
import Prelude
import qualified Graphics.GL.Core32                        as GL
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

type Ctx = [String]

makeContext :: IO Ctx
makeContext =
  words <$> (GL.glGetString GL.GL_EXTENSIONS >>= peekCString . castPtr)
