{-# OPTIONS_GHC -Wno-orphans #-}
module Web.Platform where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes.Drawing.Quad                                  (QBuffer)
import Heroes.Drawing.Utilities                             (makePaletteTexture)
import Heroes.Drawing.Utilities                             (makeTexture)
import Heroes.Platform
import Web
import Web.GLES ()
import qualified Heroes.Drawing                            as Drawing
import qualified GLES                                      as GL
import qualified Web.Audio                                 as Audio
import qualified Web.Image                                 as Image
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Control.Concurrent                                (forkIO)
import JavaScript.TypedArray.ArrayBuffer                 (ArrayBuffer)
import JavaScript.Web.Canvas                             (Canvas)
import qualified JavaScript.TypedArray                     as TypedArray
import qualified JavaScript.TypedArray.Internal.Types      as TypedArray
import qualified JavaScript.Web.XMLHttpRequest             as XHR
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

foreign import javascript unsafe
  "$r = new Float32Array([1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1])"
  createQuadArray' :: IO ArrayBuffer

foreign import javascript unsafe "$r = new Uint8Array($1);"
  freezeUint8Array :: TypedArray.IOUint8Array -> IO TypedArray.Uint8Array

instance Platform where
  --
  productionPrefix = "../.production-assets/"
  staticSpriteExtension = ".png"
  forkPreferred = forkIO
  --
  type StaticSprite = Drawing.StaticSprite
  type CursorResources = ()
  type Renderer = WebRenderer
  type InputProvider = Canvas
  --
  type ComplexSprite = Drawing.ComplexSprite
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
    return $ Drawing.ComplexSprite { .. }
  destroyComplexSprite _ = return () -- XXX
  --
  type Chunk = Audio.Audio
  loadChunk = Audio.load
  freeChunk _ = return () -- XXX
  --
  createQuadArray = createQuadArray'
  loadGLString path = do
    let request = simpleXHR path
    result <- GL.toGLString <<$>> XHR.contents <$> XHR.xhrString request
    case result of
      Nothing -> raise "result is Nothing"
      Just str -> return str

data WebRenderer = WebRenderer GL.Ctx QBuffer
