{-# OPTIONS_GHC -Wno-orphans #-}
module Native.GLES'Types () where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import GLES'Types
import Native
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Foreign.ForeignPtr                                (ForeignPtr)
import Graphics.GL.Types                                 (GLboolean)
import Graphics.GL.Types                                 (GLenum)
import Graphics.GL.Types                                 (GLfloat)
import Graphics.GL.Types                                 (GLint)
import Graphics.GL.Types                                 (GLintptr)
import Graphics.GL.Types                                 (GLsizei)
import Graphics.GL.Types                                 (GLubyte)
import Graphics.GL.Types                                 (GLuint)
import Graphics.GL.Types                                 (GLushort)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

instance GLES'Types where
  type Ctx = [String]
  type GLEnum = GLenum
  type GLUInt = GLuint
  type GLInt = GLint
  type GLPtr = Ptr ()
  type GLPtrDiff = GLintptr
  type GLSize = GLsizei
  type GLString = String
  type GLBool = GLboolean
  type Buffer = GLuint
  type UniformLocation = GLint
  type Texture = GLuint
  type Shader = GLuint
  type Program = GLuint
  type FrameBuffer = GLuint
  type RenderBuffer = GLuint
  type VertexArrayObject = GLuint
  -- type ShaderPrecisionFormat = GLint
  type AnyArray = (GLsizei, ForeignPtr ())
  type Float32Array = (GLsizei, ForeignPtr GLfloat)
  type Int32Array = (GLsizei, ForeignPtr GLint)
  type UInt16Array = (GLsizei, ForeignPtr GLushort)
  type UInt8Array = (GLsizei, ForeignPtr GLubyte)
