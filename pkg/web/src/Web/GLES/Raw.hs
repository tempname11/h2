{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
module Web.GLES.Raw where

-- WARNING: plagiarized from:
-- https://github.com/ziocroc/Ombra/blob/master/Graphics/Rendering/Ombra
-- kudos to ziocroc

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Web
import Web.GLES.Types
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import GHC.Exts                                          (Addr#)
import JavaScript.TypedArray                             (Int32Array)
import JavaScript.TypedArray                             (Float32Array)
import JavaScript.TypedArray                             (Uint8Array)
import JavaScript.TypedArray                             (Uint16Array)
import JavaScript.Web.Canvas                             (Image)
import Prelude                                           (Word)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

foreign import javascript unsafe "$1.activeTexture($2)"
        glActiveTexture :: Ctx -> Word32 -> IO ()

foreign import javascript unsafe "$1.attachShader($2, $3)"
        glAttachShader :: Ctx -> Program -> Shader -> IO ()

foreign import javascript unsafe "$1.bindAttribLocation($2, $3, $4)"
        glBindAttribLocation :: Ctx -> Program -> Word32 -> JSString -> IO ()

foreign import javascript unsafe "$1.bindBuffer($2, $3)"
        glBindBuffer :: Ctx -> Word32 -> Buffer -> IO ()

foreign import javascript unsafe "$1.bindFramebuffer($2, $3)"
        glBindFramebuffer :: Ctx -> Word32 -> FrameBuffer -> IO ()

foreign import javascript unsafe "$1.bindRenderbuffer($2, $3)"
        glBindRenderbuffer :: Ctx -> Word32 -> RenderBuffer -> IO ()

foreign import javascript unsafe "$1.bindTexture($2, $3)"
        glBindTexture :: Ctx -> Word32 -> Texture -> IO ()

foreign import javascript unsafe "$1.blendColor($2, $3, $4, $5)"
        glBlendColor :: Ctx -> Float -> Float -> Float -> Float -> IO ()

foreign import javascript unsafe "$1.blendEquation($2)"
        glBlendEquation :: Ctx -> Word32 -> IO ()

foreign import javascript unsafe "$1.blendEquationSeparate($2, $3)"
        glBlendEquationSeparate :: Ctx -> Word32 -> Word32 -> IO ()

foreign import javascript unsafe "$1.blendFunc($2, $3)"
        glBlendFunc :: Ctx -> Word32 -> Word32 -> IO ()

foreign import javascript unsafe "$1.blendFuncSeparate($2, $3, $4, $5)"
        glBlendFuncSeparate :: Ctx -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()

foreign import javascript unsafe "$1.bufferData($2, $3.buf, $4)"
        glBufferData :: Ctx -> Word32 -> Addr# -> Word32 -> IO ()

foreign import javascript unsafe "$1.bufferSubData($2, $3, $4.buf)"
        glBufferSubData :: Ctx -> Word32 -> Word -> Addr# -> IO ()

foreign import javascript unsafe "$1.checkFramebufferStatus($2)"
        glCheckFramebufferStatus :: Ctx -> Word32 -> IO Word32

foreign import javascript unsafe "$1.clear($2)"
        glClear :: Ctx -> Word32 -> IO ()

foreign import javascript unsafe "$1.clearColor($2, $3, $4, $5)"
        glClearColor :: Ctx -> Float -> Float -> Float -> Float -> IO ()

foreign import javascript unsafe "$1.clearDepth($2)"
        glClearDepth :: Ctx -> Float -> IO ()

foreign import javascript unsafe "$1.clearStencil($2)"
        glClearStencil :: Ctx -> Int32 -> IO ()

foreign import javascript unsafe "$1.colorMask($2, $3, $4, $5)"
        glColorMask :: Ctx -> Bool -> Bool -> Bool -> Bool -> IO ()

foreign import javascript unsafe "$1.compileShader($2)"
        glCompileShader :: Ctx -> Shader -> IO ()

foreign import javascript unsafe "$1.compressedTexImage2D($2, $3, $4, $5, $6, $7, $8)"
        glCompressedTexImage2D :: Ctx -> Word32 -> Int32 -> Word32 -> Int32 -> Int32 -> Int32 -> Uint8Array -> IO ()

foreign import javascript unsafe "$1.compressedTexSubImage2D($2, $3, $4, $5, $6, $7, $8, $9)"
        glCompressedTexSubImage2D :: Ctx -> Word32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Word32 -> Uint8Array -> IO ()

foreign import javascript unsafe "$1.copyTexImage2D($2, $3, $4, $5, $6, $7, $8, $9)"
        glCopyTexImage2D :: Ctx -> Word32 -> Int32 -> Word32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> IO ()

foreign import javascript unsafe "$1.copyTexSubImage2D($2, $3, $4, $5, $6, $7, $8, $9)"
        glCopyTexSubImage2D :: Ctx -> Word32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> IO ()

foreign import javascript unsafe "$1.createBuffer()"
        glCreateBuffer :: Ctx -> IO Buffer

foreign import javascript unsafe "$1.createFramebuffer()"
        glCreateFramebuffer :: Ctx -> IO FrameBuffer

foreign import javascript unsafe "$1.createProgram()"
        glCreateProgram :: Ctx -> IO Program

foreign import javascript unsafe "$1.createRenderbuffer()"
        glCreateRenderbuffer :: Ctx -> IO RenderBuffer

foreign import javascript unsafe "$1.createShader($2)"
        glCreateShader :: Ctx -> Word32 -> IO Shader

foreign import javascript unsafe "$1.createTexture()"
        glCreateTexture :: Ctx -> IO Texture

foreign import javascript unsafe "$1.cullFace($2)"
        glCullFace :: Ctx -> Word32 -> IO ()

foreign import javascript unsafe "$1.deleteBuffer($2)"
        glDeleteBuffer :: Ctx -> Buffer -> IO ()

foreign import javascript unsafe "$1.deleteFramebuffer($2)"
        glDeleteFramebuffer :: Ctx -> FrameBuffer -> IO ()

foreign import javascript unsafe "$1.deleteProgram($2)"
        glDeleteProgram :: Ctx -> Program -> IO ()

foreign import javascript unsafe "$1.deleteRenderbuffer($2)"
        glDeleteRenderbuffer :: Ctx -> RenderBuffer -> IO ()

foreign import javascript unsafe "$1.deleteShader($2)"
        glDeleteShader :: Ctx -> Shader -> IO ()

foreign import javascript unsafe "$1.deleteTexture($2)"
        glDeleteTexture :: Ctx -> Texture -> IO ()

foreign import javascript unsafe "$1.depthFunc($2)"
        glDepthFunc :: Ctx -> Word32 -> IO ()

foreign import javascript unsafe "$1.depthMask($2)"
        glDepthMask :: Ctx -> Bool -> IO ()

foreign import javascript unsafe "$1.depthRange($2, $3)"
        glDepthRange :: Ctx -> Float -> Float -> IO ()

foreign import javascript unsafe "$1.detachShader($2, $3)"
        glDetachShader :: Ctx -> Program -> Shader -> IO ()

foreign import javascript unsafe "$1.disable($2)"
        glDisable :: Ctx -> Word32 -> IO ()

foreign import javascript unsafe "$1.disableVertexAttribArray($2)"
        glDisableVertexAttribArray :: Ctx -> Word32 -> IO ()

foreign import javascript unsafe "$1.drawArrays($2, $3, $4)"
        glDrawArrays :: Ctx -> Word32 -> Int32 -> Int32 -> IO ()

foreign import javascript unsafe "$1.drawArraysInstanced($2, $3, $4, $5)"
        glDrawArraysInstanced :: Ctx -> Word32 -> Int32 -> Int32 -> Int32 -> IO ()

foreign import javascript unsafe "$1.drawElements($2, $3, $4, $5)"
        glDrawElements :: Ctx -> Word32 -> Int32 -> Word32 -> Word -> IO ()

foreign import javascript unsafe "$1.enable($2)"
        glEnable :: Ctx -> Word32 -> IO ()

foreign import javascript unsafe "$1.enableVertexAttribArray($2)"
        glEnableVertexAttribArray :: Ctx -> Word32 -> IO ()

foreign import javascript unsafe "$1.finish()"
        glFinish :: Ctx -> IO ()

foreign import javascript unsafe "$1.flush()"
        glFlush :: Ctx -> IO ()

foreign import javascript unsafe "$1.framebufferRenderbuffer($2, $3, $4, $5)"
        glFramebufferRenderbuffer :: Ctx -> Word32 -> Word32 -> Word32 -> RenderBuffer -> IO ()

foreign import javascript unsafe "$1.framebufferTexture2D($2, $3, $4, $5, $6)"
        glFramebufferTexture2D :: Ctx -> Word32 -> Word32 -> Word32 -> Texture -> Int32 -> IO ()

foreign import javascript unsafe "$1.frontFace($2)"
        glFrontFace :: Ctx -> Word32 -> IO ()

foreign import javascript unsafe "$1.generateMipmap($2)"
        glGenerateMipmap :: Ctx -> Word32 -> IO ()

foreign import javascript unsafe "$1.getActiveAttrib($2, $3)"
        glGetActiveAttrib :: Ctx -> Program -> Word32 -> IO ActiveInfo

foreign import javascript unsafe "$1.getActiveUniform($2, $3)"
        glGetActiveUniform :: Ctx -> Program -> Word32 -> IO ActiveInfo

{-
foreign import javascript unsafe "$1.getAttachedShaders($2)"
        glGetAttachedShaders :: Ctx -> Program -> IO (Sequence Shader)
-}

foreign import javascript unsafe "$1.getAttribLocation($2, $3)"
        glGetAttribLocation :: Ctx -> Program -> JSString -> IO Int32

foreign import javascript unsafe "$1.getBufferParameter($2, $3)"
        glGetBufferParameter :: Ctx -> Word32 -> Word32 -> IO JSVal

foreign import javascript unsafe "$1.getParameter($2)"
        glGetParameter :: Ctx -> Word32 -> IO JSVal

foreign import javascript unsafe "$1.getError()"
        glGetError :: Ctx -> IO Word32

foreign import javascript unsafe "$1.getFramebufferAttachmentParameter($2, $3)"
        glGetFramebufferAttachmentParameter :: Ctx -> Word32 -> Word32 -> IO Word32

foreign import javascript unsafe "$1.getProgramInfoLog($2)"
        glGetProgramInfoLog :: Ctx -> Program -> IO JSString

foreign import javascript unsafe "$1.getProgramParameter($2, $3)"
        glGetProgramParameterBool :: Ctx -> Program -> Word32 -> IO Bool

foreign import javascript unsafe "$1.getRenderbufferParameter($2, $3)"
        glGetRenderbufferParameter :: Ctx -> Word32 -> Word32 -> IO JSVal

foreign import javascript unsafe "$1.getShaderParameter($2, $3)"
        glGetShaderParameterBool :: Ctx -> Shader -> Word32 -> IO Bool

foreign import javascript unsafe "$1.getShaderPrecisionFormat($2, $3)"
        glGetShaderPrecisionFormat :: Ctx -> Word32 -> Word32 -> IO ShaderPrecisionFormat

foreign import javascript unsafe "$1.getShaderInfoLog($2)"
        glGetShaderInfoLog :: Ctx -> Shader -> IO JSString

foreign import javascript unsafe "$1.getShaderSource($2)"
        glGetShaderSource :: Ctx -> Shader -> IO JSString

foreign import javascript unsafe "$1.getTexParameter($2, $3)"
        glGetTexParameter :: Ctx -> Word32 -> Word32 -> IO JSVal

foreign import javascript unsafe "$1.getUniform($2, $3)"
        glGetUniform :: Ctx -> Program -> UniformLocation -> IO JSVal

foreign import javascript unsafe "$1.getUniformLocation($2, $3)"
        glGetUniformLocation :: Ctx -> Program -> JSString -> IO UniformLocation

foreign import javascript unsafe "$1.getVertexAttrib($2, $3)"
        glGetVertexAttrib :: Ctx -> Word32 -> Word32 -> IO JSVal

foreign import javascript unsafe "$1.getVertexAttribOffset($2, $3)"
        glGetVertexAttribOffset :: Ctx -> Word32 -> Word32 -> IO Word32

foreign import javascript unsafe "$1.hint($2, $3)"
        glHint :: Ctx -> Word32 -> Word32 -> IO ()

foreign import javascript unsafe "$1.isBuffer($2)"
        glIsBuffer :: Ctx -> Buffer -> IO Bool

foreign import javascript unsafe "$1.isEnabled($2)"
        glIsEnabled :: Ctx -> Word32 -> IO Bool

foreign import javascript unsafe "$1.isFramebuffer($2)"
        glIsFramebuffer :: Ctx -> FrameBuffer -> IO Bool

foreign import javascript unsafe "$1.isProgram($2)"
        glIsProgram :: Ctx -> Program -> IO Bool

foreign import javascript unsafe "$1.isRenderbuffer($2)"
        glIsRenderbuffer :: Ctx -> RenderBuffer -> IO Bool

foreign import javascript unsafe "$1.isShader($2)"
        glIsShader :: Ctx -> Shader -> IO Bool

foreign import javascript unsafe "$1.isTexture($2)"
        glIsTexture :: Ctx -> Texture -> IO Bool

foreign import javascript unsafe "$1.lineWidth($2)"
        glLineWidth :: Ctx -> Float -> IO ()

foreign import javascript unsafe "$1.linkProgram($2)"
        glLinkProgram :: Ctx -> Program -> IO ()

foreign import javascript unsafe "$1.pixelStorei($2, $3)"
        glPixelStorei :: Ctx -> Word32 -> Int32 -> IO ()

foreign import javascript unsafe "$1.polygonOffset($2, $3)"
        glPolygonOffset :: Ctx -> Float -> Float -> IO ()

foreign import javascript unsafe "$1.readPixels($2, $3, $4, $5, $6, $7, $8)"
        glReadPixelsUInt8 :: Ctx -> Int32 -> Int32 -> Int32 -> Int32 -> Word32 -> Word32 -> Uint8Array -> IO ()

foreign import javascript unsafe "$1.readPixels($2, $3, $4, $5, $6, $7, $8)"
        glReadPixelsUInt16 :: Ctx -> Int32 -> Int32 -> Int32 -> Int32 -> Word32 -> Word32 -> Uint16Array -> IO ()

foreign import javascript unsafe "$1.readPixels($2, $3, $4, $5, $6, $7, $8)"
        glReadPixelsFloat :: Ctx -> Int32 -> Int32 -> Int32 -> Int32 -> Word32 -> Word32 -> Float32Array -> IO ()

foreign import javascript unsafe "$1.renderbufferStorage($2, $3, $4, $5)"
        glRenderbufferStorage :: Ctx -> Word32 -> Word32 -> Int32 -> Int32 -> IO ()

foreign import javascript unsafe "$1.sampleCoverage($2, $3)"
        glSampleCoverage :: Ctx -> Float -> Bool -> IO ()

foreign import javascript unsafe "$1.scissor($2, $3, $4, $5)"
        glScissor :: Ctx -> Int32 -> Int32 -> Int32 -> Int32 -> IO ()

foreign import javascript unsafe "$1.shaderSource($2, $3)"
        glShaderSource :: Ctx -> Shader -> JSString -> IO ()

foreign import javascript unsafe "$1.stencilFunc($2, $3, $4)"
        glStencilFunc :: Ctx -> Word32 -> Int32 -> Word32 -> IO ()

foreign import javascript unsafe "$1.stencilFuncSeparate($2, $3, $4, $5)"
        glStencilFuncSeparate :: Ctx -> Word32 -> Word32 -> Int32 -> Word32 -> IO ()

foreign import javascript unsafe "$1.stencilMask($2)"
        glStencilMask :: Ctx -> Word32 -> IO ()

foreign import javascript unsafe "$1.stencilMaskSeparate($2, $3)"
        glStencilMaskSeparate :: Ctx -> Word32 -> Word32 -> IO ()

foreign import javascript unsafe "$1.stencilOp($2, $3, $4)"
        glStencilOp :: Ctx -> Word32 -> Word32 -> Word32 -> IO ()

foreign import javascript unsafe "$1.stencilOpSeparate($2, $3, $4, $5)"
        glStencilOpSeparate :: Ctx -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()

foreign import javascript unsafe "$1.texImage2D($2, $3, $4, $5, $6, $7, $8, $9, $10)"
        glTexImage2D_Image :: Ctx -> Word32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Word32 -> Word32 -> Image -> IO ()

foreign import javascript unsafe "$1.texImage2D($2, $3, $4, $5, $6, $7, $8, $9, $10.u8)"
        glTexImage2D_U8 :: Ctx -> Word32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Word32 -> Word32 -> Addr# -> IO ()

foreign import javascript unsafe "$1.texParameterf($2, $3, $4)"
        glTexParameterf :: Ctx -> Word32 -> Word32 -> Float -> IO ()

foreign import javascript unsafe "$1.texParameteri($2, $3, $4)"
        glTexParameteri :: Ctx -> Word32 -> Word32 -> Int32 -> IO ()

foreign import javascript unsafe "$1.texSubImage2D($2, $3, $4, $5, $6, $7, $8, $9, $10)"
        glTexSubImage2D :: Ctx -> Word32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Word32 -> Word32 -> Uint8Array -> IO ()

foreign import javascript unsafe "$1.uniform1f($2, $3)"
        glUniform1f :: Ctx -> UniformLocation -> Float -> IO ()

foreign import javascript unsafe "$1.uniform1fv($2, $3)"
        glUniform1fv :: Ctx -> UniformLocation -> Float32Array -> IO ()

foreign import javascript unsafe "$1.uniform1i($2, $3)"
        glUniform1i :: Ctx -> UniformLocation -> Int32 -> IO ()

foreign import javascript unsafe "$1.uniform1iv($2, $3)"
        glUniform1iv :: Ctx -> UniformLocation -> Int32Array -> IO ()

foreign import javascript unsafe "$1.uniform2f($2, $3, $4)"
        glUniform2f :: Ctx -> UniformLocation -> Float -> Float -> IO ()

foreign import javascript unsafe "$1.uniform2fv($2, $3)"
        glUniform2fv :: Ctx -> UniformLocation -> Float32Array -> IO ()

foreign import javascript unsafe "$1.uniform2i($2, $3, $4)"
        glUniform2i :: Ctx -> UniformLocation -> Int32 -> Int32 -> IO ()

foreign import javascript unsafe "$1.uniform2iv($2, $3)"
        glUniform2iv :: Ctx -> UniformLocation -> Int32Array -> IO ()

foreign import javascript unsafe "$1.uniform3f($2, $3, $4, $5)"
        glUniform3f :: Ctx -> UniformLocation -> Float -> Float -> Float -> IO ()

foreign import javascript unsafe "$1.uniform3fv($2, $3)"
        glUniform3fv :: Ctx -> UniformLocation -> Float32Array -> IO ()

foreign import javascript unsafe "$1.uniform3i($2, $3, $4, $5)"
        glUniform3i :: Ctx -> UniformLocation -> Int32 -> Int32 -> Int32 -> IO ()

foreign import javascript unsafe "$1.uniform3iv($2, $3)"
        glUniform3iv :: Ctx -> UniformLocation -> Int32Array -> IO ()

foreign import javascript unsafe "$1.uniform4f($2, $3, $4, $5, $6)"
        glUniform4f :: Ctx -> UniformLocation -> Float -> Float -> Float -> Float -> IO ()

foreign import javascript unsafe "$1.uniform4fv($2, $3)"
        glUniform4fv :: Ctx -> UniformLocation -> Float32Array -> IO ()

foreign import javascript unsafe "$1.uniform4i($2, $3, $4, $5, $6)"
        glUniform4i :: Ctx -> UniformLocation -> Int32 -> Int32 -> Int32 -> Int32 -> IO ()

foreign import javascript unsafe "$1.uniform4iv($2, $3)"
        glUniform4iv :: Ctx -> UniformLocation -> Int32Array -> IO ()

foreign import javascript unsafe "$1.useProgram($2)"
        glUseProgram :: Ctx -> Program -> IO ()

foreign import javascript unsafe "$1.validateProgram($2)"
        glValidateProgram :: Ctx -> Program -> IO ()

foreign import javascript unsafe "$1.vertexAttrib1f($2, $3)"
        glVertexAttrib1f :: Ctx -> Word32 -> Float -> IO ()

foreign import javascript unsafe "$1.vertexAttrib1fv($2, $3)"
        glVertexAttrib1fv :: Ctx -> Word32 -> Float32Array -> IO ()

foreign import javascript unsafe "$1.vertexAttrib2f($2, $3, $4)"
        glVertexAttrib2f :: Ctx -> Word32 -> Float -> Float -> IO ()

foreign import javascript unsafe "$1.vertexAttrib2fv($2, $3)"
        glVertexAttrib2fv :: Ctx -> Word32 -> Float32Array -> IO ()

foreign import javascript unsafe "$1.vertexAttrib3f($2, $3, $4, $5)"
        glVertexAttrib3f :: Ctx -> Word32 -> Float -> Float -> Float -> IO ()

foreign import javascript unsafe "$1.vertexAttrib3fv($2, $3)"
        glVertexAttrib3fv :: Ctx -> Word32 -> Float32Array -> IO ()

foreign import javascript unsafe "$1.vertexAttrib4f($2, $3, $4, $5, $6)"
        glVertexAttrib4f :: Ctx -> Word32 -> Float -> Float -> Float -> Float -> IO ()

foreign import javascript unsafe "$1.vertexAttrib4fv($2, $3)"
        glVertexAttrib4fv :: Ctx -> Word32 -> Float32Array -> IO ()

foreign import javascript unsafe "$1.vertexAttribPointer($2, $3, $4, $5, $6, $7)"
        glVertexAttribPointer :: Ctx -> Word32 -> Int32 -> Word32 -> Bool -> Int32 -> Word -> IO ()

foreign import javascript unsafe "$1.vertexAttribDivisor($2, $3)"
        glVertexAttribDivisor :: Ctx -> Word32 -> Word32 -> IO ()

foreign import javascript unsafe "$1.viewport($2, $3, $4, $5)"
        glViewport :: Ctx -> Int32 -> Int32 -> Int32 -> Int32 -> IO ()

-- Extensions

foreign import javascript unsafe "$1.getExtension($2)"
        getExtension :: Ctx -> JSString -> IO JSVal

-- OES_vertex_array_object

foreign import javascript unsafe "$1.vaoExt.createVertexArrayOES()"
        glCreateVertexArrayOES :: Ctx -> IO VertexArrayObject

foreign import javascript unsafe "$1.vaoExt.bindVertexArrayOES($2)"
        glBindVertexArrayOES :: Ctx -> VertexArrayObject -> IO()

foreign import javascript unsafe "$1.vaoExt.deleteVertexArrayOES($2)"
        glDeleteVertexArrayOES :: Ctx -> VertexArrayObject -> IO()

foreign import javascript unsafe "$1.vaoExt.isVertexArrayOES($2)"
        glIsVertexArrayOES :: Ctx -> VertexArrayObject -> IO Bool

-- WEBGL_draw_buffers

foreign import javascript unsafe "$1.drawBufs.drawBuffersWEBGL($2)"
        glDrawBuffersWEBGL :: Ctx -> Int32Array -> IO ()
