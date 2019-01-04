{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- WARNING: plagiarized from:
-- https://github.com/ziocroc/Ombra/blob/master/Graphics/Rendering/Ombra
-- kudos to ziocroc

module Web.GLES.Types (
  Ctx,
  Program,
  Shader,
  Buffer,
  FrameBuffer,
  RenderBuffer,
  VertexArrayObject,
  Texture,
  UniformLocation,
  ActiveInfo,
  ShaderPrecisionFormat,
  noBuffer,
  noFramebuffer,
  noTexture,
  noVAO,
) where

import GHCJS.Foreign
import GHCJS.Types

type Ctx = JSVal

type Program = JSVal

type Shader = JSVal

type Buffer = JSVal

type FrameBuffer = JSVal

type RenderBuffer = JSVal

type VertexArrayObject = JSVal

type Texture = JSVal

{-
instance Eq Texture where
        (==) = eqRef
-}

type UniformLocation = JSVal

type ActiveInfo = JSVal

type ShaderPrecisionFormat = JSVal

-- type ArrayBufferView = JSVal

noBuffer :: Buffer
noBuffer = jsNull

noFramebuffer :: FrameBuffer
noFramebuffer = jsNull

noTexture :: Texture
noTexture = jsNull

noVAO :: VertexArrayObject
noVAO = jsNull
