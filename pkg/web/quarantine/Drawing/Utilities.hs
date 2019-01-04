module Web.Drawing.Utilities where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Web
import qualified Web.GLES                                  as GL
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.JSString                             as JSString
import JavaScript.TypedArray.Internal.Types              (IOUint8Array)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

bindTextureTo :: GL.Context -> GL.GLEnum -> GL.Texture -> IO ()
bindTextureTo ctx slot texture = do
  GL.activeTexture ctx slot
  GL.bindTexture ctx GL.gl_TEXTURE_2D texture
--------------------------------------------------------------------------------
makeProgram :: GL.Context -> String -> String -> IO GL.Program
makeProgram ctx fragmentFilename vertexFilename = do
  fragmentSource <- loadString fragmentFilename
  vertexSource   <- loadString vertexFilename

  fs <- makeShader ctx GL.gl_FRAGMENT_SHADER fragmentSource
  vs <- makeShader ctx GL.gl_VERTEX_SHADER vertexSource

  program <- GL.createProgram ctx

  GL.attachShader ctx program fs
  GL.attachShader ctx program vs
  GL.linkProgram ctx program
  GL.detachShader ctx program fs
  GL.detachShader ctx program vs

  GL.deleteShader ctx fs -- this is safe,
                         -- all we care about is the compiled program.
  GL.deleteShader ctx vs

  status <- GL.getProgramParameter ctx program GL.gl_LINK_STATUS >>= fromJSVal
  when (status == Nothing || status == (Just 0 :: Maybe Int)) $
    raise "bad program link status"

  return program
--------------------------------------------------------------------------------
makeShader :: GL.Context -> GL.GLEnum -> String -> IO GL.Shader
makeShader ctx type_ source = do
  shader <- GL.createShader ctx type_
  GL.shaderSource ctx shader (JSString.pack source)
  GL.compileShader ctx shader

  status <- GL.getShaderParameter ctx shader GL.gl_COMPILE_STATUS >>= fromJSVal
  when (status == Nothing || status == (Just 0 :: Maybe Int)) $ do
    info <- JSString.unpack <$> GL.getShaderInfoLog ctx shader
    raise ("bad shader compile status: " <> info)

  return shader
--------------------------------------------------------------------------------
makePaletteTexture :: GL.Context -> IOUint8Array -> IO GL.Texture
makePaletteTexture ctx array = do
  texture <- GL.createTexture ctx
  GL.bindTexture ctx GL.gl_TEXTURE_2D texture
  GL.texImage2D_xx_array ctx GL.gl_TEXTURE_2D
    0 GL.gl_RGBA 256 1 0 GL.gl_RGBA GL.gl_UNSIGNED_BYTE array
  GL.texParameteri ctx GL.gl_TEXTURE_2D
    ((§) GL.gl_TEXTURE_MAG_FILTER) ((§) GL.gl_NEAREST)
  GL.texParameteri ctx GL.gl_TEXTURE_2D
    ((§) GL.gl_TEXTURE_MIN_FILTER) ((§) GL.gl_NEAREST)
  -- GL.unbindTexture
  return texture
--------------------------------------------------------------------------------
makeTexture :: GL.Context -> Image -> IO GL.Texture
makeTexture ctx image = do
  texture <- GL.createTexture ctx
  GL.bindTexture ctx GL.gl_TEXTURE_2D texture

  GL.texImage2D_image ctx GL.gl_TEXTURE_2D
    0 GL.gl_RGBA GL.gl_RGBA GL.gl_UNSIGNED_BYTE image

  GL.texParameteri ctx GL.gl_TEXTURE_2D
    ((§) GL.gl_TEXTURE_MAG_FILTER) ((§) GL.gl_NEAREST)

  GL.texParameteri ctx GL.gl_TEXTURE_2D
    ((§) GL.gl_TEXTURE_MIN_FILTER) ((§) GL.gl_NEAREST)

  do -- non-power-of-2 stuff:
    GL.texParameteri ctx GL.gl_TEXTURE_2D
      ((§) GL.gl_TEXTURE_WRAP_S) ((§) GL.gl_CLAMP_TO_EDGE);

    GL.texParameteri ctx GL.gl_TEXTURE_2D
      ((§) GL.gl_TEXTURE_WRAP_T) ((§) GL.gl_CLAMP_TO_EDGE);

  -- GL.unbindTexture
  return texture
