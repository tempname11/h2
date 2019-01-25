{-# OPTIONS_GHC -Wno-orphans #-}
module Web.Platform where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes.Platform
import Web
import Web.GLES ()
import qualified GLES                                      as GL
import qualified Web.Audio                                 as Audio
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Control.Concurrent                                (forkIO)
import JavaScript.TypedArray.ArrayBuffer                 (ArrayBuffer)
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
