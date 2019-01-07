{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Web.Platform where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Web
import Web.GLES ()
import Web.Drawing.Quad                                  (QBuffer)
import Heroes.Platform
import Heroes.SpriteMeta                                 (Meta)
import qualified GLES                                      as GL
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Control.Concurrent                                (forkIO)
import JavaScript.Web.Canvas                             (Canvas)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

instance Platform where
  --
  productionPrefix = "../.production-assets/"
  staticSpriteExtension = ".png"
  forkPreferred = forkIO
  --
  type StaticSprite = WebStaticSprite
  type CursorResources = ()
  type Renderer = WebRenderer
  type InputProvider = Canvas
  --
  type ComplexSprite = WebComplexSprite
  loadComplexSprite = fix id -- XXX
  destroyComplexSprite = fix id -- XXX
  --
  type Chunk = Audio
  loadChunk = fix id -- XXX
  freeChunk = fix id -- XXX

data WebStaticSprite = WebStaticSprite {
  texture    :: GL.Texture,
  dimensions :: V2 Float
}

data WebComplexSprite = WebComplexSprite {
  atlasTexture :: GL.Texture,
  paletteTexture :: GL.Texture,
  meta :: Meta
}

data WebRenderer = WebRenderer GL.Ctx QBuffer

newtype Audio = Audio JSVal
instance IsJSVal Audio

instance Show WebComplexSprite where
  show _ = "WebComplexSprite" -- XXX

makeShorthands ''WebStaticSprite
makeShorthands ''WebComplexSprite
