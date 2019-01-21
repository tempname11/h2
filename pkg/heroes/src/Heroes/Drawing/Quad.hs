module Heroes.Drawing.Quad (
  QBuffer (..),
  createBuffer,
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import GLES                                              (GLES)
import Heroes
import Heroes.Platform                                   (Platform)
import qualified Heroes.Platform                           as Platform
import qualified GLES                                      as GL
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

newtype QBuffer = QBuffer GL.Buffer

createBuffer :: (Platform, GLES) => GL.Ctx -> IO QBuffer
createBuffer ctx = do
  array <- Platform.createQuadArray
  buffer <- GL.glCreateBuffer ctx
  GL.glBindBuffer ctx GL.gl_ARRAY_BUFFER buffer
  GL.glBufferData ctx GL.gl_ARRAY_BUFFER array GL.gl_STATIC_DRAW
  GL.glBindBuffer ctx GL.gl_ARRAY_BUFFER GL.noBuffer
  return $ QBuffer buffer
