module Web.Artifacts.Prototype1.Drawing where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes.UI                                         (viewportSize)
import Web
import Web.Artifacts.Prototype1.Drawing.RPPB
import qualified GLES                                      as GL
import qualified Heroes.Essentials                         as Essentials
import qualified Heroes.H3                                 as H3
import qualified Heroes.SpriteMeta                         as Meta
import qualified Web.Artifacts.Prototype1.Drawing.Paletted as Paletted
import qualified Web.Artifacts.Prototype1.Drawing.Regular  as Regular
import qualified Web.GLES                                  as GL
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import JavaScript.TypedArray.ArrayBuffer                  (ArrayBuffer)
import qualified Data.JSString                            as JSString
import qualified Data.Vector                              as V
import qualified JavaScript.TypedArray                    as TypedArray
import qualified JavaScript.TypedArray.Internal.Types     as TypedArray
import qualified JavaScript.Web.Canvas                    as Canvas
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

foreign import javascript unsafe
  "$r = new Float32Array([1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1])"
  niftyArray :: IO ArrayBuffer -- might be the wrong type!!!

--------------------------------------------------------------------------------

data Static = Static {
  ctx    :: GL.Ctx,
  pProg  :: Paletted.Prog,
  rProg  :: Regular.Prog,
  buffer :: GL.Buffer,
  sprite :: SpriteData,
  bg     :: GL.Texture
}

data SpriteData = SpriteData {
  atlasTexture   :: GL.Texture,
  paletteTexture :: GL.Texture,
  meta           :: Meta.Meta
}

data PalettedDrawCmd = PalettedDrawCmd { -- bad name?
  sprite :: SpriteData,
  rpcb   :: RPCB
}

data RPCB = RPCB { -- Regular & Paletted Command Bits
  -- (meaning the common data between them)
  dimensions  :: V2 Float,
  box         :: V2 Float,
  place       :: Point V2 Float,
  screenPlace :: Point V2 Float
}

data RegularDrawCmd = RegularDrawCmd { -- bad name?
  texture :: GL.Texture,
  rpcb    :: RPCB
}

--------------------------------------------------------------------------------
-- utilities

makeShader :: GL.Ctx -> GL.GLEnum -> String -> IO GL.Shader
makeShader ctx type_ source = do
  shader <- GL.glCreateShader ctx type_
  GL.glShaderSource ctx shader (JSString.pack source)
  GL.glCompileShader ctx shader
  --
  status <- GL.glGetShaderParameterBool ctx shader GL.gl_COMPILE_STATUS
  when (not status) $ do
    info <- JSString.unpack <$> GL.glGetShaderInfoLog ctx shader
    raise ("bad shader compile status: " <> info)
  --
  return shader

makeProgram :: GL.Ctx -> String -> String -> IO GL.Program
makeProgram ctx fragmentFilename vertexFilename = do
  fragmentSource <- loadString fragmentFilename
  vertexSource   <- loadString vertexFilename
  --
  fs <- makeShader ctx GL.gl_FRAGMENT_SHADER fragmentSource
  vs <- makeShader ctx GL.gl_VERTEX_SHADER vertexSource
  --
  program <- GL.glCreateProgram ctx
  GL.glAttachShader ctx program fs
  GL.glAttachShader ctx program vs
  GL.glLinkProgram ctx program
  --
  status <- GL.glGetProgramParameterBool ctx program GL.gl_LINK_STATUS
  when (not status) $
    raise "bad program link status"
  --
  return program

makeVBO :: GL.Ctx -> IO GL.Buffer
makeVBO ctx = do
  array <- niftyArray
  buffer <- GL.glCreateBuffer ctx
  GL.glBindBuffer ctx GL.gl_ARRAY_BUFFER buffer
  GL.glBufferData ctx GL.gl_ARRAY_BUFFER array GL.gl_STATIC_DRAW
  -- GL.unbindBuffer
  return buffer

makePaletteTexture :: GL.Ctx -> GL.UInt8Array -> IO GL.Texture
makePaletteTexture ctx array = do
  texture <- GL.glCreateTexture ctx
  GL.glBindTexture ctx GL.gl_TEXTURE_2D texture
  GL.glTexImage2DUInt ctx GL.gl_TEXTURE_2D
    0 ((§) GL.gl_RGBA) 256 1 0 GL.gl_RGBA GL.gl_UNSIGNED_BYTE array
  GL.glTexParameteri ctx GL.gl_TEXTURE_2D
    ((§) GL.gl_TEXTURE_MAG_FILTER) ((§) GL.gl_NEAREST)
  GL.glTexParameteri ctx GL.gl_TEXTURE_2D
    ((§) GL.gl_TEXTURE_MIN_FILTER) ((§) GL.gl_NEAREST)
  GL.glTexParameteri ctx GL.gl_TEXTURE_2D
    ((§) GL.gl_TEXTURE_WRAP_S) ((§) GL.gl_CLAMP_TO_EDGE);
  GL.glTexParameteri ctx GL.gl_TEXTURE_2D
    ((§) GL.gl_TEXTURE_WRAP_T) ((§) GL.gl_CLAMP_TO_EDGE);
  -- GL.unbindTexture
  return texture

makeTexture :: GL.Ctx -> Image -> IO GL.Texture
makeTexture ctx image = do
  texture <- GL.glCreateTexture ctx
  GL.glBindTexture ctx GL.gl_TEXTURE_2D texture
  GL.glTexImage2DImage ctx GL.gl_TEXTURE_2D
    0 ((§) GL.gl_RGBA) GL.gl_RGBA GL.gl_UNSIGNED_BYTE image
  GL.glTexParameteri ctx GL.gl_TEXTURE_2D
    ((§) GL.gl_TEXTURE_MAG_FILTER) ((§) GL.gl_NEAREST)
  GL.glTexParameteri ctx GL.gl_TEXTURE_2D
    ((§) GL.gl_TEXTURE_MIN_FILTER) ((§) GL.gl_NEAREST)
  GL.glTexParameteri ctx GL.gl_TEXTURE_2D
    ((§) GL.gl_TEXTURE_WRAP_S) ((§) GL.gl_CLAMP_TO_EDGE);
  GL.glTexParameteri ctx GL.gl_TEXTURE_2D
    ((§) GL.gl_TEXTURE_WRAP_T) ((§) GL.gl_CLAMP_TO_EDGE);
  -- GL.unbindTexture
  return texture

bindTextureTo :: GL.Ctx -> GL.GLEnum -> GL.Texture -> IO ()
bindTextureTo ctx slot texture = do
  GL.glActiveTexture ctx slot
  GL.glBindTexture ctx GL.gl_TEXTURE_2D texture

repeatingPart :: GL.Ctx -> GL.Program -> GL.Buffer -> GL.GLUInt -> IO ()
repeatingPart ctx program buffer attr_interp = do
  GL.glUseProgram ctx program
  GL.glBindBuffer ctx GL.gl_ARRAY_BUFFER buffer
  GL.glEnableVertexAttribArray ctx attr_interp
  GL.glVertexAttribPointer ctx attr_interp 2 GL.gl_FLOAT False 0 0

--------------------------------------------------------------------------------
-- the "meat"

ready :: GL.Ctx -> IO ()
ready ctx = do
  GL.glClearColor ctx 0 0 0 1
  GL.glClear ctx GL.gl_COLOR_BUFFER_BIT

-- "regular"

regularReady :: Static -> IO ()
regularReady static = do
  --
  let Static ctx _ prog buffer _ _ = static
      Regular.Prog { program, rppb } = prog
      RPPB { attr_interp } = rppb
  --
  repeatingPart ctx program buffer attr_interp


regularDraw :: Static -> RegularDrawCmd -> IO ()
regularDraw static command = do
  --
  let Static ctx _ prog _ _ _ = static
      Regular.Prog { rppb, .. } = prog
      RPPB { .. } = rppb
      RegularDrawCmd texture rpcb = command
      RPCB dimensions box place screenPlace = rpcb
      vsize = viewportSize <&> (§)
  -- set uniforms
  GL.glUniform1i ctx loc_texImage 0
  GL.glUniform2f ctx loc_texDimensions (dimensions ^. _x) (dimensions ^. _y)
  GL.glUniform2f ctx loc_scrDimensions (vsize ^. _x) (vsize ^. _y)
  GL.glUniform2f ctx loc_texPlace (place ^. _x) (place ^. _y)
  GL.glUniform2f ctx loc_scrPlace (screenPlace ^. _x) (screenPlace ^. _y)
  GL.glUniform2f ctx loc_scrBox (box ^. _x) (box ^. _y)
  GL.glUniform2f ctx loc_texBox (box ^. _x) (box ^. _y)
  -- bind textures
  bindTextureTo ctx GL.gl_TEXTURE0 texture
  -- draw call!
  GL.glDrawArrays ctx GL.gl_TRIANGLES 0 6

palettedReady :: Static -> IO ()
palettedReady static = do
  --
  let Static ctx prog _ buffer _ _ = static
      Paletted.Prog { program, rppb } = prog
      RPPB { attr_interp } = rppb
  --
  repeatingPart ctx program buffer attr_interp

palettedDraw :: Static -> PalettedDrawCmd -> IO ()
palettedDraw static command = do
  -- needs the world to be in a `Ready State`
  let Static ctx prog _ _ _ _ = static
      SpriteData atlasTexture paletteTexture _ = sprite
      Paletted.Prog { rppb, .. } = prog
      RPPB { .. } = rppb
      PalettedDrawCmd sprite rpcb = command
      RPCB dimensions box place screenPlace = rpcb
      vsize = viewportSize <&> (§)
  -- set uniforms
  GL.glUniform1i ctx loc_texAtlas 0
  GL.glUniform1i ctx loc_texPalette 1
  GL.glUniform2f ctx loc_texDimensions (dimensions ^. _x) (dimensions ^. _y)
  GL.glUniform2f ctx loc_scrDimensions (vsize ^. _x) (vsize ^. _y)
  GL.glUniform2f ctx loc_texPlace (place ^. _x) (place ^. _y)
  GL.glUniform2f ctx loc_scrPlace (screenPlace ^. _x) (screenPlace ^. _y)
  GL.glUniform2f ctx loc_scrBox (box ^. _x) (box ^. _y)
  GL.glUniform2f ctx loc_texBox (box ^. _x) (box ^. _y)
  -- bind textures
  bindTextureTo ctx GL.gl_TEXTURE0 atlasTexture
  bindTextureTo ctx GL.gl_TEXTURE1 paletteTexture
  -- draw call!
  GL.glDrawArrays ctx GL.gl_TRIANGLES 0 6

--------------------------------------------------------------------------------
-- exported procedures

run :: Double -> Static -> IO ()
run timer static = do
  --
  let Static ctx _ _ _ sprite bg = static
      SpriteData _ _ meta = sprite
  --
  let groups = meta ^. groups_
      group = groups ! 0
      n = V.length group
      frame = group ! frameN
      frameN = floor (timer * 0.015) `mod` n
      offset = (<§>) (frame ^. offset_)
  --
  let dimensions = (<§>) (meta ^. dimensions_)
      box = (<§>) (frame ^. box_)
      place = (<§>) (frame ^. place_)
      screenPlace = P (V2 400 300) .+^ offset
  --
  ready ctx
  --
  let commandR = RegularDrawCmd bg (RPCB 1024 size 0 0)
        where size = (<§>) viewportSize
  regularReady static
  regularDraw static commandR
  --
  let commandP = PalettedDrawCmd sprite (RPCB dimensions box place screenPlace)
  palettedReady static
  palettedDraw static commandP

foreign import javascript unsafe "$r = new Uint8Array($1);"
  freezeUint8Array :: TypedArray.IOUint8Array -> IO TypedArray.Uint8Array

init :: Canvas -> Image -> Image -> Essentials.Essentials -> IO Static
init canvas image bg essentials = do
  ctx <- GL.getWebGLContext canvas
  --
  w <- (§) <$> Canvas.width canvas
  h <- (§) <$> Canvas.height canvas
  GL.glViewport ctx 0 0 w h
  GL.glEnable ctx GL.gl_BLEND
  GL.glBlendFunc ctx GL.gl_SRC_ALPHA GL.gl_ONE_MINUS_SRC_ALPHA
  --
  palettedProg <- makeProgram ctx
    "../glsl/paletted.fragment.glsl"
    "../glsl/paletted.vertex.glsl"
    >>= Paletted.toProg ctx
  --
  regularProg <- makeProgram ctx
    "../glsl/regular.fragment.glsl"
    "../glsl/regular.vertex.glsl"
    >>= Regular.toProg ctx
  --
  buffer  <- makeVBO ctx
  atlasTexture <- makeTexture ctx image
  bgTexture <- makeTexture ctx bg
  --
  let
    Essentials.Essentials { creatureMeta } = essentials
    meta = creatureMeta H3.WoodElf
    palette = meta ^. palette_
  --
  paletteArray' <- TypedArray.create 1024
  for_ [0..255] $ \i -> do
    let V4 r g b a = if i > 8
                     then (<§>) (palette ! i)
                     else 0
    TypedArray.unsafeSetIndex (i * 4 + 0) r paletteArray'
    TypedArray.unsafeSetIndex (i * 4 + 1) g paletteArray'
    TypedArray.unsafeSetIndex (i * 4 + 2) b paletteArray'
    TypedArray.unsafeSetIndex (i * 4 + 3) a paletteArray'
  --
  paletteArray <- freezeUint8Array paletteArray'
  paletteTexture <- makePaletteTexture ctx paletteArray
  --
  return $ Static ctx palettedProg regularProg buffer (SpriteData {..}) bgTexture
