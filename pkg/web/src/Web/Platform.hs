{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Web.Platform where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Web
import Heroes.Platform
import Heroes.SpriteMeta                                 (Meta)
import qualified GLES                                      as GL
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Control.Concurrent                                (forkIO)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

instance Platform where
  productionPrefix = "../.production-assets/"
  staticSpriteExtension = ".png"
  type StaticSprite = WebStaticSprite
  type ComplexSprite = WebComplexSprite
  type Chunk = Audio
  forkPreferred = forkIO

data WebStaticSprite = WebStaticSprite {
  texture    :: GL.Texture,
  dimensions :: V2 Float
}

data WebComplexSprite = WebComplexSprite {
  atlasTexture :: GL.Texture,
  paletteTexture :: GL.Texture,
  meta :: Meta
}

newtype Audio = Audio JSVal
instance IsJSVal Audio

makeShorthands ''WebStaticSprite
makeShorthands ''WebComplexSprite
