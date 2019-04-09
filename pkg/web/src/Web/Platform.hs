{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE JavaScriptFFI #-}
module Web.Platform where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes.Platform
import Web
import Web.Image ()
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Control.Concurrent                                (forkIO)
import JavaScript.Web.Canvas                             (Image)
import qualified Data.JSString                             as JSString
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

instance Platform where
  productionPrefix = "../.production-assets/"
  forkPreferred = forkIO
  --
  loadImage path = do
    img <- newImage
    setImageSrc img (JSString.pack path)
    waitTillImageLoads img
    return (Right img) -- XXX errors are not handled
  --

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

