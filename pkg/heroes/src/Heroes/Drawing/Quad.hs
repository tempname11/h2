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
import qualified Data.Vector.Storable                      as SV
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

newtype QBuffer = QBuffer GL.Buffer

createBuffer :: (GLX.GLX, GLES) => GL.Ctx -> IO QBuffer
createBuffer ctx = do
  let array = SV.fromList @Float [1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1]
  buffer <- GL.glCreateBuffer ctx
  -- XXX delete it afterwards!
  GL.glBindBuffer ctx GL.gl_ARRAY_BUFFER buffer
  GL.glBufferData ctx GL.gl_ARRAY_BUFFER (unsafeToBuf array) GL.gl_STATIC_DRAW
  GL.glBindBuffer ctx GL.gl_ARRAY_BUFFER GL.noBuffer
  return $ QBuffer buffer
