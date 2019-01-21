module Heroes.Drawing.Utilities where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import GLES                                              (GLES)
import Heroes.Platform                                   (Platform)
import Heroes
import qualified GLES                                      as GL
import qualified Heroes.Platform                           as Platform
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

bindTextureTo :: GLES => GL.Ctx -> GL.GLEnum -> GL.Texture -> IO ()
bindTextureTo ctx slot texture = do
  GL.glActiveTexture ctx slot
  GL.glBindTexture ctx GL.gl_TEXTURE_2D texture

makeProgram :: (Platform, GLES) => GL.Ctx -> String -> String -> IO GL.Program
makeProgram ctx fragmentFilename vertexFilename = do
  fragmentSource <- Platform.loadGLString fragmentFilename
  vertexSource <- Platform.loadGLString vertexFilename
  --
  fs <- makeShader ctx GL.gl_FRAGMENT_SHADER fragmentSource
  vs <- makeShader ctx GL.gl_VERTEX_SHADER vertexSource
  --
  program <- GL.glCreateProgram ctx
  GL.glAttachShader ctx program fs
  GL.glAttachShader ctx program vs
  GL.glLinkProgram ctx program
  GL.glDetachShader ctx program fs
  GL.glDetachShader ctx program vs
  GL.glDeleteShader ctx fs -- this is safe
  GL.glDeleteShader ctx vs
  --
  status <- GL.glGetProgramParameterBool ctx program GL.gl_LINK_STATUS
  when (not (GL.isTrue status)) $
    raise "bad program link status"
  --
  return program

makeShader :: GLES => GL.Ctx -> GL.GLEnum -> GL.GLString -> IO GL.Shader
makeShader ctx type_ source = do
  shader <- GL.glCreateShader ctx type_
  GL.glShaderSource ctx shader source
  GL.glCompileShader ctx shader
  --
  status <- GL.glGetShaderParameterBool ctx shader GL.gl_COMPILE_STATUS
  when (not (GL.isTrue status)) $ do
    info <- GL.glGetShaderInfoLog ctx shader
    raise ("bad shader compile status: " <> GL.fromGLString info)
  --
  return shader

makePaletteTexture :: GLES => GL.Ctx -> GL.UInt8Array -> IO GL.Texture
makePaletteTexture ctx array = do
  texture <- GL.glCreateTexture ctx
  GL.glBindTexture ctx GL.gl_TEXTURE_2D texture
  GL.glTexImage2DUInt ctx GL.gl_TEXTURE_2D
    0 ((§) GL.gl_RGBA) 256 1 0 GL.gl_RGBA GL.gl_UNSIGNED_BYTE array
  GL.glTexParameteri ctx GL.gl_TEXTURE_2D
    ((§) GL.gl_TEXTURE_MAG_FILTER) ((§) GL.gl_NEAREST)
  GL.glTexParameteri ctx GL.gl_TEXTURE_2D
    ((§) GL.gl_TEXTURE_MIN_FILTER) ((§) GL.gl_NEAREST)
  -- XXX GL.unbindTexture?
  return texture

makeTexture :: GLES => GL.Ctx -> GL.Image -> IO GL.Texture
makeTexture ctx image = do
  texture <- GL.glCreateTexture ctx
  GL.glBindTexture ctx GL.gl_TEXTURE_2D texture
  --
  GL.glTexImage2DImage ctx GL.gl_TEXTURE_2D
    0 ((§) GL.gl_RGBA) GL.gl_RGBA GL.gl_UNSIGNED_BYTE image
  --
  GL.glTexParameteri ctx GL.gl_TEXTURE_2D
    ((§) GL.gl_TEXTURE_MAG_FILTER) ((§) GL.gl_NEAREST)
  --
  GL.glTexParameteri ctx GL.gl_TEXTURE_2D
    ((§) GL.gl_TEXTURE_MIN_FILTER) ((§) GL.gl_NEAREST)
  --
  do -- non-power-of-2 stuff:
    GL.glTexParameteri ctx GL.gl_TEXTURE_2D
      ((§) GL.gl_TEXTURE_WRAP_S) ((§) GL.gl_CLAMP_TO_EDGE);
    --
    GL.glTexParameteri ctx GL.gl_TEXTURE_2D
      ((§) GL.gl_TEXTURE_WRAP_T) ((§) GL.gl_CLAMP_TO_EDGE);
  -- XXX GL.unbindTexture?
  return texture
