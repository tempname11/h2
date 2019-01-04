module Web.Drawing.Quad (
  QBuffer (..),
  createBuffer,
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Web
import qualified Web.GLES                                  as GL
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import JavaScript.TypedArray.ArrayBuffer (ArrayBuffer)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

newtype QBuffer = QBuffer GL.Buffer

--------------------------------------------------------------------------------

foreign import javascript unsafe
  "$r = new Float32Array([1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1])"
  createQuadArray :: IO ArrayBuffer

createBuffer :: GL.Context -> IO QBuffer
createBuffer ctx = do
  array <- createQuadArray
  GLX.runGL ctx $ do
    buffer <- GLX.createBuffer
    GLX.bindBuffer GL.gl_ARRAY_BUFFER buffer
    GLX.bufferData GL.gl_ARRAY_BUFFER array GL.gl_STATIC_DRAW
    GLX.bindBuffer_null GL.gl_ARRAY_BUFFER
    return $ QBuffer buffer
