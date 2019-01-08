{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Web.Platform where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Web
import Web.GLES ()
import Web.Drawing.Quad                                  (QBuffer)
import Web.Drawing.Utilities                             (makeTexture)
import Web.Drawing.Utilities                             (makePaletteTexture)
import Heroes.Platform
import Heroes.SpriteMeta                                 (Meta)
import qualified GLES                                      as GL
import qualified Web.Audio                                 as Audio
import qualified Web.Image                                 as Image
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Control.Concurrent                                (forkIO)
import JavaScript.Web.Canvas                             (Canvas)
import qualified JavaScript.TypedArray                     as TypedArray
import qualified JavaScript.TypedArray.Internal.Types      as TypedArray
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

foreign import javascript unsafe "$r = new Uint8Array($1);"
  freezeUint8Array :: TypedArray.IOUint8Array -> IO TypedArray.Uint8Array

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
  loadComplexSprite (WebRenderer ctx _) meta path = do
    image <- Image.load path
    atlasTexture <- makeTexture ctx image
    let palette = meta ^. palette_
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
    return $ WebComplexSprite { .. }
  destroyComplexSprite _ = return () -- XXX
  --
  type Chunk = Audio.Audio
  loadChunk = Audio.load
  freeChunk _ = return () -- XXX

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

instance Show WebComplexSprite where
  show _ = "WebComplexSprite" -- XXX

makeShorthands ''WebStaticSprite
makeShorthands ''WebComplexSprite
