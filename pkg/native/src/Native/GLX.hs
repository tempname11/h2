{-# OPTIONS_GHC -Wno-orphans #-}
module Native.GLX where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes.GLX
import Native
import Native.GLES ()
import Native.WND'SDL ()
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Foreign.Ptr                                       (castPtr)
import Foreign.C.String                                  (peekCString)
import SDL                                               (($=))
import System.IO                                         (readFile)
import qualified Graphics.GL.Core33                        as GL
import qualified SDL
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

instance GLX where
  --
  loadGLSL = readFile
  --
  getGLContext window = do
    void $ SDL.glCreateContext window
    SDL.swapInterval $= SDL.SynchronizedUpdates
    words <$> (GL.glGetString GL.GL_EXTENSIONS >>= peekCString . castPtr)
