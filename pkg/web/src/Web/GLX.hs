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
import qualified JavaScript.Web.XMLHttpRequest             as XHR
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

instance GLX where
  --
  getGLContext = getWebGLContext
  --
  loadGLSL path = do
    let request = simpleXHR path
    result <- GL.toGLString <<$>> XHR.contents <$> XHR.xhrString request
    case result of
      Nothing -> raise "result is Nothing"
      Just str -> return (shaderPrefix <> str)

shaderPrefix :: JSString
shaderPrefix = JSString.pack "precision mediump float;\n"
