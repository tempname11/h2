{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE JavaScriptFFI #-}
module Web.Platform where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes.Platform
import Web
import Web.WND'Canvas ()
import Web.GLES                                          (getWebGLContext)
import qualified GLES                                      as GL
import qualified Web.Audio                                 as Audio
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Control.Concurrent                                (forkIO)
import JavaScript.TypedArray.ArrayBuffer                 (ArrayBuffer)
import JavaScript.Web.Canvas                             (Image)
import qualified Data.JSString                             as JSString
import qualified JavaScript.TypedArray                     as TypedArray
import qualified JavaScript.TypedArray.Internal.Types      as TypedArray'
import qualified JavaScript.Web.XMLHttpRequest             as XHR
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

foreign import javascript unsafe
  "$r = new Float32Array([1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1])"
  createQuadArray' :: IO ArrayBuffer

foreign import javascript unsafe
  "$r = new Uint8Array($1);"
  freezeUint8Array :: TypedArray'.IOUint8Array -> IO TypedArray.Uint8Array

instance Platform where
  productionPrefix = "../.production-assets/"
  staticSpriteExtension = ".png"
  forkPreferred = forkIO
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
  getGLContext = getWebGLContext
  loadImage path = do
    img <- newImage
    setImageSrc img (JSString.pack path)
    waitTillImageLoads img
    return (Right img) -- XXX errors are not handled
  generatePaletteArray palette = do
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
    freezeUint8Array paletteArray'

foreign import javascript unsafe "new Image()"
  newImage :: IO Image

foreign import javascript unsafe "$1.src = $2"
  setImageSrc :: Image -> JSString -> IO ()

foreign import javascript interruptible
  " \
  \ if ($1.complete) {  \
  \   setImmediate($c); \
  \ } else {            \
  \   $1.onload = $c    \
  \ }                   \
  \ "
  waitTillImageLoads :: Image -> IO ()

