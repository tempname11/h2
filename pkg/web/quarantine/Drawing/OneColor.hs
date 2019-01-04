module Web.Drawing.OneColor (
  with,
  Cmd (..),
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Common.With
import Heroes.UI (viewportSize)
import Web
import Web.Drawing.Utilities
import qualified Web.Drawing.Quad                          as Quad
import qualified Web.GLES                                  as GL
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.JSString                             as JSString
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Cmd = Cmd {
  box :: Maybe (V2 Float),
  place :: Point V2 Float,
  color :: V4 Float
}

--------------------------------------------------------------------------------

with :: GL.Context -> Quad.QBuffer -> With5 Cmd
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
  attr_interp       :: Word32
}

--------------------------------------------------------------------------------

init :: GL.Context -> IO Prog
init ctx = do
  program <- makeProgram ctx
    "../glsl/one-color.fragment.glsl"
    "../glsl/one-color.vertex.glsl"

  attr_interp <- (§) <$> -- Int32 vs Word32 for some reason
    GL.getAttribLocation ctx program (JSString.pack "interp")

  let locate name = GL.getUniformLocation ctx program (JSString.pack name)

  loc_scrDimensions <- locate "scrDimensions"
  loc_scrPlace      <- locate "scrPlace"
  loc_scrBox        <- locate "scrBox"
  loc_color         <- locate "color"

  return $ Prog { .. }


fini :: GL.Context -> Prog -> IO ()
fini ctx prog = do
  let Prog { program } = prog
  GL.deleteProgram ctx program


ready :: Quad.QBuffer -> GL.Context -> Prog -> IO ()
ready qBuffer ctx prog = do
  let Prog { .. } = prog
      Quad.QBuffer buffer = qBuffer
  GL.useProgram ctx program
  GL.bindBuffer ctx GL.gl_ARRAY_BUFFER buffer
  GL.enableVertexAttribArray ctx attr_interp
  GL.vertexAttribPointer ctx attr_interp 2 GL.gl_FLOAT False 0 0


draw :: GL.Context -> Prog -> Cmd -> IO ()
draw ctx prog cmd = do
  let Prog { .. } = prog
      Cmd { box, place, color } = cmd
      vsize = viewportSize <&> (§)

  GL.uniform2f ctx loc_scrDimensions (vsize ^. _x) (vsize ^. _y)
  GL.uniform2f ctx loc_scrPlace (place ^. _x) (place ^. _y)
  let b = maybe vsize id box
  GL.uniform2f ctx loc_scrBox (b ^. _x) (b ^. _y)
  let c = color
  GL.uniform4f ctx loc_color (c ^. _x) (c ^. _y) (c ^. _z) (c ^. _w)

  -- draw call!
  GL.drawArrays ctx GL.gl_TRIANGLES 0 6
