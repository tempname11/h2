module Heroes.Drawing.Regular (
  with,
  Cmd (..),
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Common.With
import GLES                                              (GLES)
import Heroes
import Heroes.Drawing
import Heroes.Drawing.Utilities
import Heroes.FilePath                                   (prod)
import Heroes.Platform                                   (Platform)
import Heroes.UI (viewportSize)
import qualified GLES                                      as GL
import qualified Heroes.GLX                                as GLX
import qualified Heroes.Drawing.Quad                       as Quad
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.Vector.Storable                      as SV
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Cmd = Cmd {
  sprite :: StaticSprite,
  box :: V2 Float,
  screenBox :: V2 Float,
  place :: Point V2 Float,
  screenPlaces :: SV.Vector (Point V2 Float)
}

--------------------------------------------------------------------------------

with :: (Platform, GLX.GLX, GLES) => GL.Ctx -> Quad.QBuffer -> With2 (Handler Cmd)
with ctx qBuffer = \next2 -> do
  (prog, screenPlaceBuffer) <- init ctx
  next2 $ \next1 -> do
    ready qBuffer ctx prog screenPlaceBuffer
    --
    next1 $
      draw ctx prog screenPlaceBuffer
    --
    unready ctx prog
  fini ctx prog

--------------------------------------------------------------------------------

data Prog = Prog {
  program      :: GL.Program,
  loc_texImage :: GL.UniformLocation,
  loc_texDimensions :: GL.UniformLocation,
  loc_scrDimensions :: GL.UniformLocation,
  loc_texPlace      :: GL.UniformLocation,
  loc_texBox        :: GL.UniformLocation,
  loc_scrBox        :: GL.UniformLocation,
  attr_interp       :: GL.GLUInt,
  attr_scrPlace     :: GL.GLUInt
}

--------------------------------------------------------------------------------

init :: (Platform, GLX.GLX, GLES) => GL.Ctx -> IO (Prog, GL.Buffer)
init ctx = do
  program <- makeProgram ctx
    (prod <> "glsl/regular.fragment.glsl")
    (prod <> "glsl/regular.vertex.glsl")
  --
  attr_interp <- (§) <$> -- Int32 vs Word32 for some reason
    GL.glGetAttribLocation ctx program (GL.toGLString "interp")
  --
  attr_scrPlace <- (§) <$> -- Int32 vs Word32 for some reason
    GL.glGetAttribLocation ctx program (GL.toGLString "scrPlace")
  --
  let locate name = GL.glGetUniformLocation ctx program (GL.toGLString name)
  --
  loc_texDimensions <- locate "texDimensions"
  loc_scrDimensions <- locate "scrDimensions"
  loc_texPlace      <- locate "texPlace"
  loc_texBox        <- locate "texBox"
  loc_scrBox        <- locate "scrBox"
  loc_texImage      <- locate "texImage"
  --
  screenPlaceBuffer <- GL.glCreateBuffer ctx
  -- XXX delete it afterwards!
  GL.glBindBuffer ctx GL.gl_ARRAY_BUFFER screenPlaceBuffer
  --
  return (Prog { .. }, screenPlaceBuffer)

fini :: GLES => GL.Ctx -> Prog -> IO ()
fini ctx prog = do
  let Prog { program } = prog
  GL.glDeleteProgram ctx program

unready :: GLES => GL.Ctx -> Prog -> IO ()
unready ctx prog = do
  let Prog { attr_interp, attr_scrPlace } = prog
  GL.glDisableVertexAttribArray ctx attr_interp
  GL.glDisableVertexAttribArray ctx attr_scrPlace
  GL.glVertexAttribDivisor ctx attr_scrPlace 0

ready :: GLES => Quad.QBuffer -> GL.Ctx -> Prog -> GL.Buffer -> IO ()
ready qBuffer ctx prog screenPlaceBuffer = do
  let Prog { program, attr_interp, attr_scrPlace } = prog
  GL.glUseProgram ctx program
  --
  do
    let Quad.QBuffer buffer = qBuffer
    GL.glBindBuffer ctx GL.gl_ARRAY_BUFFER buffer
    GL.glEnableVertexAttribArray ctx attr_interp
    GL.glVertexAttribPointer ctx attr_interp 2 GL.gl_FLOAT GL.false 0 GL.nullGLPtr
  --
  GL.glBindBuffer ctx GL.gl_ARRAY_BUFFER screenPlaceBuffer
  GL.glEnableVertexAttribArray ctx attr_scrPlace
  GL.glVertexAttribPointer ctx attr_scrPlace 2 GL.gl_FLOAT GL.false 0 GL.nullGLPtr
  GL.glVertexAttribDivisor ctx attr_scrPlace 1
  --
  GL.glBindBuffer ctx GL.gl_ARRAY_BUFFER GL.noBuffer

draw :: GLES => GL.Ctx -> Prog -> GL.Buffer -> Cmd -> IO ()
draw ctx prog screenPlaceBuffer cmd = do
  let Prog { .. } = prog
      Cmd {
        sprite = StaticSprite { texture, dimensions },
        box,
        screenBox,
        place,
        screenPlaces
      } = cmd
      vsize = viewportSize <&> (§)
  --
  GL.glUniform1i ctx loc_texImage 0
  GL.glUniform2f ctx loc_texDimensions (dimensions ^. _x) (dimensions ^. _y)
  GL.glUniform2f ctx loc_scrDimensions (vsize ^. _x) (vsize ^. _y)
  GL.glUniform2f ctx loc_texPlace (place ^. _x) (place ^. _y)
  GL.glUniform2f ctx loc_texBox (box ^. _x) (box ^. _y)
  GL.glUniform2f ctx loc_scrBox (screenBox ^. _x) (screenBox ^. _y)
  --
  GL.glActiveTexture ctx GL.gl_TEXTURE0 
  GL.glBindTexture ctx GL.gl_TEXTURE_2D texture
  --
  GL.glBindBuffer ctx GL.gl_ARRAY_BUFFER screenPlaceBuffer
  GL.glBufferData ctx GL.gl_ARRAY_BUFFER (unsafeToBuf screenPlaces) GL.gl_DYNAMIC_DRAW
  GL.glDrawArraysInstanced ctx GL.gl_TRIANGLES 0 6 ((§) (SV.length screenPlaces))

