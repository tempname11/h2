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

data Cmd = Cmd {
  sprite :: StaticSprite,
  box         :: V2 Float,
  screenBox   :: V2 Float,
  place       :: Point V2 Float,
  screenPlaces :: [Point V2 Float]
}

--------------------------------------------------------------------------------

with :: (Platform, GLX.GLX, GLES) => GL.Ctx -> Quad.QBuffer -> With2 (Handler Cmd)
with ctx qBuffer = \next2 -> do
  prog <- init ctx
  next2 $ \next1 -> do
    ready qBuffer ctx prog
    next1 $ draw ctx prog
  fini ctx prog

--------------------------------------------------------------------------------

data Prog = Prog {
  program      :: GL.Program,
  loc_texImage :: GL.UniformLocation,
  loc_texDimensions :: GL.UniformLocation,
  loc_scrDimensions :: GL.UniformLocation,
  loc_texPlace      :: GL.UniformLocation,
  loc_scrPlace      :: GL.UniformLocation,
  loc_texBox        :: GL.UniformLocation,
  loc_scrBox        :: GL.UniformLocation,
  attr_interp       :: GL.GLUInt
}

--------------------------------------------------------------------------------

init :: (Platform, GLX.GLX, GLES) => GL.Ctx -> IO Prog
init ctx = do
  program <- makeProgram ctx
    (prod <> "glsl/regular.fragment.glsl")
    (prod <> "glsl/regular.vertex.glsl")
  --
  attr_interp <- (§) <$> -- Int32 vs Word32 for some reason
    GL.glGetAttribLocation ctx program (GL.toGLString "interp")
  --
  let locate name = GL.glGetUniformLocation ctx program (GL.toGLString name)
  --
  loc_texDimensions <- locate "texDimensions"
  loc_scrDimensions <- locate "scrDimensions"
  loc_texPlace      <- locate "texPlace"
  loc_scrPlace      <- locate "scrPlace"
  loc_texBox        <- locate "texBox"
  loc_scrBox        <- locate "scrBox"
  loc_texImage      <- locate "texImage"
  --
  return $ Prog { .. }

fini :: GLES => GL.Ctx -> Prog -> IO ()
fini ctx prog = do
  let Prog { program } = prog
  GL.glDeleteProgram ctx program

ready :: GLES => Quad.QBuffer -> GL.Ctx -> Prog -> IO ()
ready qBuffer ctx prog = do
  let Prog { program, attr_interp } = prog
      Quad.QBuffer buffer = qBuffer
  GL.glUseProgram ctx program
  GL.glBindBuffer ctx GL.gl_ARRAY_BUFFER buffer
  GL.glEnableVertexAttribArray ctx attr_interp
  GL.glVertexAttribPointer ctx attr_interp 2 GL.gl_FLOAT GL.false 0 GL.nullGLPtr

draw :: GLES => GL.Ctx -> Prog -> Cmd -> IO ()
draw ctx prog cmd = do
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
  GL.glActiveTexture ctx GL.gl_TEXTURE0 
  GL.glBindTexture ctx GL.gl_TEXTURE_2D texture
  --
  for_ screenPlaces $ \screenPlace -> do
    GL.glUniform2f ctx loc_scrPlace (screenPlace ^. _x) (screenPlace ^. _y)
    GL.glDrawArrays ctx GL.gl_TRIANGLES 0 6
