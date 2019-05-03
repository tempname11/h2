{-# LANGUAGE FlexibleContexts #-}
module GLES'Types where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Common
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

type GLSize = Int32
type GLInt = Int32
type GLEnum = Word32
type GLUInt = Word32

class (Num GLPtrDiff, Eq GLPtrDiff) => GLES'Types where
  type Ctx
  type GLPtr
  type GLPtrDiff
  type GLString
  type GLBool
  type Buffer
  type UniformLocation
  type Texture
  type Shader
  type Program
  type FrameBuffer
  type RenderBuffer
  type VertexArrayObject
  -- XXX arrays
  type AnyArray
  type Float32Array
  type Int32Array
  type UInt8Array
  type UInt16Array
