module Heroes.Drawing.Quad (
  QBuffer (..),
  createBuffer,
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import GLES                                              (GLES)
import Heroes
import qualified Heroes.GLX                                as GLX
import qualified GLES                                      as GL
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

newtype QBuffer = QBuffer GL.Buffer

createBuffer :: (GLX.GLX, GLES) => GL.Ctx -> IO QBuffer
createBuffer ctx = do
  array <- GLX.createQuadArray
  buffer <- GL.glCreateBuffer ctx
  -- XXX delete it afterwards!
  GL.glBindBuffer ctx GL.gl_ARRAY_BUFFER buffer
  GL.glBufferDataA ctx GL.gl_ARRAY_BUFFER array GL.gl_STATIC_DRAW
  GL.glBindBuffer ctx GL.gl_ARRAY_BUFFER GL.noBuffer
  return $ QBuffer buffer
