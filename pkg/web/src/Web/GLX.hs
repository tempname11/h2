{-# OPTIONS_GHC -Wno-orphans #-}
module Web.GLX where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes.GLX
import Web
import Web.GLES                                          (getWebGLContext)
import Web.WND'Canvas ()
import qualified GLES                                      as GL
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.JSString                             as JSString
import qualified JavaScript.TypedArray                     as TypedArray
import qualified JavaScript.TypedArray.Internal.Types      as TypedArray'
import qualified JavaScript.Web.XMLHttpRequest             as XHR
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

instance GLX where
  --
  createQuadArray = createQuadArray'
  getGLContext = getWebGLContext
  --
  loadGLSL path = do
    let request = simpleXHR path
    result <- GL.toGLString <<$>> XHR.contents <$> XHR.xhrString request
    case result of
      Nothing -> raise "result is Nothing"
      Just str -> return (shaderPrefix <> str)
  --
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

foreign import javascript unsafe
  "$r = new Float32Array([1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1])"
  createQuadArray' :: IO JSVal

foreign import javascript unsafe
  "$r = new Uint8Array($1);"
  freezeUint8Array :: TypedArray'.IOUint8Array -> IO TypedArray.Uint8Array

shaderPrefix :: JSString
shaderPrefix = JSString.pack "precision mediump float;\n"
