module Heroes.Drawing.Text (
  with,
  prepare,
  Cmd (..),
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Common.With
import GLES                                              (GLES)
import Heroes
import Heroes.Color                                      (Color)
import Heroes.Drawing                                    (FontAtlas(..))
import Heroes.Drawing.Utilities
import Heroes.FilePath                                   (prod)
import Heroes.FontMeta                                   (GlyphMeta(..))
import Heroes.Platform                                   (Platform)
import Heroes.UI (viewportSize)
import qualified GLES                                      as GL
import qualified Heroes.GLX                                as GLX
import qualified Heroes.Drawing.Quad                       as Quad
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.ByteString                           as B
import qualified Data.Vector.Storable                      as SV
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Cmd = Cmd {
  screenPlace :: Point V2 Float,
  color :: Color,
  prepared :: Prepared
}

data Prepared = Prepared {
  fontAtlas :: FontAtlas,
  boxes :: SV.Vector (V2 Float),
  offsets :: SV.Vector (V2 Float),
  texPlaces :: SV.Vector (V2 Float),
  count :: GL.GLSize
}

data Self = Self {
  program :: GL.Program,
  uniform_texImage :: GL.UniformLocation,
  uniform_texDimensions :: GL.UniformLocation,
  uniform_scrDimensions :: GL.UniformLocation,
  uniform_scrPlace :: GL.UniformLocation,
  uniform_color :: GL.UniformLocation,
  attribute_interp :: GL.GLUInt,
  attribute_texPlace :: GL.GLUInt,
  attribute_offset :: GL.GLUInt,
  attribute_box :: GL.GLUInt,
  buffer_offsets :: GL.Buffer,
  buffer_texPlaces :: GL.Buffer,
  buffer_boxes :: GL.Buffer
}

with :: (Platform, GLX.GLX, GLES) => GL.Ctx -> Quad.QBuffer -> With2 (Handler Cmd)
with ctx qBuffer = \next2 -> do
  self <- init ctx
  next2 $ \next1 -> do
    ready qBuffer ctx self
    next1 $ draw ctx self
    unready ctx self
  --
  fini ctx self

init :: (Platform, GLX.GLX, GLES) => GL.Ctx -> IO Self
init ctx = do
  program <- makeProgram ctx
    (prod <> "glsl/text.fragment.glsl")
    (prod <> "glsl/text.vertex.glsl")
  --
  let
    locateUniform name = GL.glGetUniformLocation ctx program (GL.toGLString name)
    locateAttrib name = (§) <$> -- Int32 vs Word32 for some reason
      GL.glGetAttribLocation ctx program (GL.toGLString name)
  --
  attribute_interp <- locateAttrib "interp"
  attribute_offset <- locateAttrib "offset"
  attribute_texPlace <- locateAttrib "texPlace"
  attribute_box <- locateAttrib "box"
  --
  uniform_texDimensions <- locateUniform "texDimensions"
  uniform_scrDimensions <- locateUniform "scrDimensions"
  uniform_scrPlace <- locateUniform "scrPlace"
  uniform_texImage <- locateUniform "texImage"
  uniform_color <- locateUniform "color"
  --
  buffer_offsets <- GL.glCreateBuffer ctx
  buffer_texPlaces <- GL.glCreateBuffer ctx
  buffer_boxes <- GL.glCreateBuffer ctx
  -- XXX delete them afterwards!
  return (Self {..})

fini :: GLES => GL.Ctx -> Self -> IO ()
fini ctx self = do
  let Self { program } = self
  GL.glDeleteProgram ctx program

unready :: GLES => GL.Ctx -> Self -> IO ()
unready ctx (Self {..}) = do
  GL.glDisableVertexAttribArray ctx attribute_interp
  GL.glDisableVertexAttribArray ctx attribute_offset
  GL.glDisableVertexAttribArray ctx attribute_texPlace
  GL.glDisableVertexAttribArray ctx attribute_box
  GL.glVertexAttribDivisor ctx attribute_offset 0
  GL.glVertexAttribDivisor ctx attribute_texPlace 0
  GL.glVertexAttribDivisor ctx attribute_box 0
  GL.glBindBuffer ctx GL.gl_ARRAY_BUFFER GL.noBuffer

ready :: GLES => Quad.QBuffer -> GL.Ctx -> Self -> IO ()
ready qBuffer ctx (Self {..}) = do
  GL.glUseProgram ctx program
  --
  do
    let Quad.QBuffer buffer = qBuffer
    GL.glBindBuffer ctx GL.gl_ARRAY_BUFFER buffer
    GL.glEnableVertexAttribArray ctx attribute_interp
    GL.glVertexAttribPointer ctx attribute_interp 2 GL.gl_FLOAT GL.false 0 GL.nullGLPtr
  --
  for_
    [
      (buffer_offsets, attribute_offset),
      (buffer_texPlaces, attribute_texPlace),
      (buffer_boxes, attribute_box)
    ] $ \(buffer, attribute) -> do
      GL.glBindBuffer ctx GL.gl_ARRAY_BUFFER buffer
      GL.glEnableVertexAttribArray ctx attribute
      GL.glVertexAttribPointer ctx attribute 2 GL.gl_FLOAT GL.false 0 GL.nullGLPtr
      GL.glVertexAttribDivisor ctx attribute 1
  --
  let vsize = (<§>) viewportSize
  GL.glUniform1i ctx uniform_texImage 0
  GL.glUniform2f ctx uniform_scrDimensions (vsize ^. _x) (vsize ^. _y)

prepare :: FontAtlas -> ByteString -> (V2 Float, Prepared)
prepare fontAtlas string = (measurements, Prepared {..})
  where
  FontAtlas { meta } = fontAtlas
  glyphs = meta ^. #glyphs
  ascender = (§) (meta ^. #ascender)
  descender = (§) (meta ^. #descender)
  gs = B.unpack string <&> \code -> glyphs ! (§) code
  count = (§) (B.length string)
  boxes = SV.fromList (gs <&> (<§>) . view #box)
  texPlaces = SV.fromList (gs <&> (<§>) . view #place)
  offsets = SV.fromList $ reverse $ fst result
  measurements = V2 (snd result) (ascender - descender)
  result = -- XXX looks pretty horrible
    foldl'
      (\(rs, a) (GlyphMeta { advanceX, renderOffset }) -> (
        ((<§>) renderOffset .+^ V2 a ascender) : rs,
        a + (§) advanceX
      ))
      ([], 0)
      gs

draw :: GLES => GL.Ctx -> Self -> Cmd -> IO ()
draw ctx (Self {..}) (Cmd {..}) = do
  let
    FontAtlas { meta, texture } = fontAtlas
    Prepared {..} = prepared
    dimensions = (<§>) (meta ^. #dimensions)
  --
  GL.glUniform2f ctx uniform_texDimensions (dimensions ^. _x) (dimensions ^. _y)
  GL.glUniform2f ctx uniform_scrPlace (screenPlace ^. _x) (screenPlace ^. _y)
  GL.glUniform4f ctx uniform_color
    ((§) (color ^. _x))
    ((§) (color ^. _y))
    ((§) (color ^. _z))
    ((§) (color ^. _w))
  --
  GL.glActiveTexture ctx GL.gl_TEXTURE0 
  GL.glBindTexture ctx GL.gl_TEXTURE_2D texture
  --
  GL.glBindBuffer ctx GL.gl_ARRAY_BUFFER buffer_offsets
  GL.glBufferData ctx GL.gl_ARRAY_BUFFER (unsafeToBuf offsets) GL.gl_DYNAMIC_DRAW
  --
  GL.glBindBuffer ctx GL.gl_ARRAY_BUFFER buffer_texPlaces
  GL.glBufferData ctx GL.gl_ARRAY_BUFFER (unsafeToBuf texPlaces) GL.gl_DYNAMIC_DRAW
  --
  GL.glBindBuffer ctx GL.gl_ARRAY_BUFFER buffer_boxes
  GL.glBufferData ctx GL.gl_ARRAY_BUFFER (unsafeToBuf boxes) GL.gl_DYNAMIC_DRAW
  --
  GL.glDrawArraysInstanced ctx GL.gl_TRIANGLES 0 6 count
