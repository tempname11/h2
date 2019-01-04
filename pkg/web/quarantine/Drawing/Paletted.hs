module Web.Drawing.Paletted (
  with,
  Cmd (..),
) where

-- XXX: this file is very similar to Regular.hs

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Common.With
import Heroes.UI (viewportSize)
import Web
import Web.ComplexSprite                                 (ComplexSprite(..))
import Web.Drawing
import Web.Drawing.Utilities
import qualified Web.Drawing.Quad                          as Quad
import qualified Web.GLES                                  as GL
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.JSString                             as JSString
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Cmd = Cmd {
  sprite   :: ComplexSprite,
  copySpec :: CopySpec
}

--------------------------------------------------------------------------------

with :: GL.Context -> Quad.QBuffer -> With5 Cmd
with ctx qBuffer next0 = do
  prog <- init ctx
  next0 $ \next1 -> do
    ready qBuffer ctx prog
    next1 $ \cmd -> do
      draw ctx prog cmd
  fini ctx prog

--------------------------------------------------------------------------------

data Prog = Prog {
  program        :: GL.Program,
  loc_texAtlas   :: GL.UniformLocation,
  loc_texPalette :: GL.UniformLocation,
  loc_texDimensions :: GL.UniformLocation,
  loc_scrDimensions :: GL.UniformLocation,
  loc_texPlace      :: GL.UniformLocation,
  loc_scrPlace      :: GL.UniformLocation,
  loc_texBox        :: GL.UniformLocation,
  loc_scrBox        :: GL.UniformLocation,
  attr_interp       :: Word32
}

--------------------------------------------------------------------------------

init :: GL.Context -> IO Prog
init ctx = do
  program <- makeProgram ctx
    "../glsl/paletted.fragment.glsl"
    "../glsl/paletted.vertex.glsl"

  attr_interp <- (§) <$> -- Int32 vs Word32 for some reason
    GL.getAttribLocation ctx program (JSString.pack "interp")

  let locate name = GL.getUniformLocation ctx program (JSString.pack name)

  loc_texDimensions <- locate "texDimensions"
  loc_scrDimensions <- locate "scrDimensions"
  loc_texPlace      <- locate "texPlace"
  loc_scrPlace      <- locate "scrPlace"
  loc_texBox        <- locate "texBox"
  loc_scrBox        <- locate "scrBox"
  loc_texAtlas      <- locate "texAtlas"
  loc_texPalette    <- locate "texPalette"

  return $ Prog { .. }


fini :: GL.Context -> Prog -> IO ()
fini ctx prog = do
  let Prog { program } = prog
  GL.deleteProgram ctx program


ready :: Quad.QBuffer -> GL.Context -> Prog -> IO ()
ready qBuffer ctx prog = do
  let Prog { program, attr_interp } = prog
      Quad.QBuffer buffer = qBuffer
  GL.useProgram ctx program
  GL.bindBuffer ctx GL.gl_ARRAY_BUFFER buffer
  GL.enableVertexAttribArray ctx attr_interp
  GL.vertexAttribPointer ctx attr_interp 2 GL.gl_FLOAT False 0 0


draw :: GL.Context -> Prog -> Cmd -> IO ()
draw ctx prog cmd = do
  let Prog { .. } = prog
      Cmd {
        sprite = ComplexSprite {
          atlasTexture = atlas,
          paletteTexture = palette,
          meta
        },
        copySpec = CopySpec { place, screenPlace, box, screenBox }
      } = cmd
      vsize = viewportSize <&> (§)

  let dimensions = (<§>) (meta ^. dimensions_)
  GL.uniform1i ctx loc_texAtlas 0
  GL.uniform1i ctx loc_texPalette 1
  GL.uniform2f ctx loc_texDimensions (dimensions ^. _x) (dimensions ^. _y)
  GL.uniform2f ctx loc_scrDimensions (vsize ^. _x) (vsize ^. _y)
  GL.uniform2f ctx loc_texPlace (place ^. _x) (place ^. _y)
  GL.uniform2f ctx loc_scrPlace (screenPlace ^. _x) (screenPlace ^. _y)
  GL.uniform2f ctx loc_texBox (box ^. _x) (box ^. _y)
  GL.uniform2f ctx loc_scrBox (screenBox ^. _x) (screenBox ^. _y)

  -- bind textures
  bindTextureTo ctx GL.gl_TEXTURE0 atlas
  bindTextureTo ctx GL.gl_TEXTURE1 palette

  -- draw call!
  GL.drawArrays ctx GL.gl_TRIANGLES 0 6
