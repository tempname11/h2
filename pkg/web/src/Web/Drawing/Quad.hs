module Web.Drawing.Quad (
  QBuffer (..),
  createBuffer,
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import GLES                                              (GLES)
import Web
import Web.GLES ()
import qualified GLES                                      as GL
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import JavaScript.TypedArray.ArrayBuffer (ArrayBuffer)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

newtype QBuffer = QBuffer GL.Buffer

--------------------------------------------------------------------------------

foreign import javascript unsafe
  "$r = new Float32Array([1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1])"
  createQuadArray :: IO ArrayBuffer

createBuffer :: GLES => GL.Ctx -> IO QBuffer
createBuffer ctx = do
  array <- createQuadArray
  buffer <- GL.glCreateBuffer ctx
  GL.glBindBuffer ctx GL.gl_ARRAY_BUFFER buffer
  GL.glBufferData ctx GL.gl_ARRAY_BUFFER array GL.gl_STATIC_DRAW
  GL.glBindBuffer ctx GL.gl_ARRAY_BUFFER GL.noBuffer
  return $ QBuffer buffer
