{-# LANGUAGE JavaScriptFFI #-}
module Web.Image where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Web
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.JSString                             as JSString
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

foreign import javascript unsafe "new Image()"
  new :: IO Image

foreign import javascript unsafe "$1.src = $2"
  setSrc :: Image -> JSString -> IO ()

foreign import javascript unsafe "$1.naturalWidth"
  width :: Image -> IO Int

foreign import javascript unsafe "$1.naturalHeight"
  height :: Image -> IO Int

foreign import javascript interruptible
  " \
  \ if ($1.complete) {  \
  \   setImmediate($c); \
  \ } else {            \
  \   $1.onload = $c    \
  \ }                   \
  \ "
  waitTillLoads :: Image -> IO ()

load :: String -> IO Image
load path = do
  img <- new
  setSrc img (JSString.pack path)
  waitTillLoads img
  return img
