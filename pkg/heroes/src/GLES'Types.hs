{-# LANGUAGE FlexibleContexts #-}
module GLES'Types where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Common
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Data.Bits                                         (Bits)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

class
  ( 
    Integral GLEnum,
    Integral GLUInt,
    Integral GLInt,
    Integral GLSize,
    Bits GLEnum,
    Num GLEnum,
    Num GLUInt,
    Num GLInt,
    Num GLPtrDiff,
    Num GLSize,
    Eq GLEnum,
    Eq GLUInt,
    Eq GLInt,
    Eq GLPtrDiff,
    Eq GLSize
  ) => GLES'Types where
  type Ctx
  type GLEnum
  type GLUInt
  type GLInt
  type GLPtr
  type GLPtrDiff
  type GLSize
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
  -- type ActiveInfo
  -- type ShaderPrecisionFormat
  -- XXX arrays
  type AnyArray
  type Float32Array
  type Int32Array
  type UInt8Array
  type UInt16Array
