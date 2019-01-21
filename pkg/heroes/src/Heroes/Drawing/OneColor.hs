module Heroes.Drawing.OneColor (
  with,
  Cmd (..),
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Common.With
import GLES                                              (GLES)
import Heroes
import Heroes.Drawing.Utilities
import Heroes.Platform                                   (Platform)
import Heroes.UI (viewportSize)
import qualified GLES                                      as GL
import qualified Heroes.Drawing.Quad                       as Quad
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Cmd = Cmd {
  box :: Maybe (V2 Float),
  place :: Point V2 Float,
  color :: V4 Float
}

--------------------------------------------------------------------------------

with :: (Platform, GLES) => GL.Ctx -> Quad.QBuffer -> With2 (Handler Cmd)
with ctx qBuffer = \next2 -> do
  prog <- init ctx
  next2 $ \next1 -> do
    ready qBuffer ctx prog
    next1 $ \cmd -> do
      draw ctx prog cmd
  fini ctx prog

--------------------------------------------------------------------------------

data Prog = Prog {
  program           :: GL.Program,
  loc_color         :: GL.UniformLocation,
  loc_scrPlace      :: GL.UniformLocation,
  loc_scrDimensions :: GL.UniformLocation,
  loc_scrBox        :: GL.UniformLocation,
  attr_interp       :: GL.GLUInt
}

--------------------------------------------------------------------------------

init :: (Platform, GLES) => GL.Ctx -> IO Prog
init ctx = do
  program <- makeProgram ctx
    "../glsl/one-color.fragment.glsl"
    "../glsl/one-color.vertex.glsl"
  --
  attr_interp <- (§) <$> -- Int32 vs Word32 for some reason
    GL.glGetAttribLocation ctx program (GL.toGLString "interp")
  --
  let locate name = GL.glGetUniformLocation ctx program (GL.toGLString name)
  --
  loc_scrDimensions <- locate "scrDimensions"
  loc_scrPlace      <- locate "scrPlace"
  loc_scrBox        <- locate "scrBox"
  loc_color         <- locate "color"
  --
  return $ Prog { .. }

fini :: GLES => GL.Ctx -> Prog -> IO ()
fini ctx prog = do
  let Prog { program } = prog
  GL.glDeleteProgram ctx program

ready :: GLES => Quad.QBuffer -> GL.Ctx -> Prog -> IO ()
ready qBuffer ctx prog = do
  let Prog { .. } = prog
      Quad.QBuffer buffer = qBuffer
  --
  GL.glUseProgram ctx program
  GL.glBindBuffer ctx GL.gl_ARRAY_BUFFER buffer
  GL.glEnableVertexAttribArray ctx attr_interp
  GL.glVertexAttribPointer ctx attr_interp 2 GL.gl_FLOAT GL.false 0 GL.nullGLPtr

draw :: GLES => GL.Ctx -> Prog -> Cmd -> IO ()
draw ctx prog cmd = do
  let Prog { .. } = prog
      Cmd { box, place, color } = cmd
      vsize = viewportSize <&> (§)
  --
  GL.glUniform2f ctx loc_scrDimensions (vsize ^. _x) (vsize ^. _y)
  GL.glUniform2f ctx loc_scrPlace (place ^. _x) (place ^. _y)
  let
  --
    b = maybe vsize id box
    c = color
  --
  GL.glUniform2f ctx loc_scrBox (b ^. _x) (b ^. _y)
  GL.glUniform4f ctx loc_color (c ^. _x) (c ^. _y) (c ^. _z) (c ^. _w)
  -- draw call!
  GL.glDrawArrays ctx GL.gl_TRIANGLES 0 6
