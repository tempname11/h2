{-# LANGUAGE JavaScriptFFI #-}
module Web.Audio where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Web
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.JSString                             as JSString
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

newtype Audio = Audio JSVal
instance IsJSVal Audio

foreign import javascript unsafe "new Audio()"
  new :: IO Audio

foreign import javascript unsafe "$1.src = $2"
  setSrc :: Audio -> JSString -> IO ()

foreign import javascript unsafe "$1.loop = false; $1.play();"
  playOnce :: Audio -> IO ()

foreign import javascript unsafe "$1.loop = true; $1.play();"
  start :: Audio -> IO ()

foreign import javascript unsafe "$1.pause(); $1.currentTime = 0;"
  stop :: Audio -> IO ()

foreign import javascript interruptible
  " \
  \ if ($1.readyState > 3) {    \
  \   setImmediate($c);         \
  \ } else {                    \
  \   $1.oncanplaythrough = $c; \
  \   $1.load();                \
  \ }                           \
  \ "
  waitTillLoads :: Audio -> IO ()

load :: String -> IO Audio
load path = do
  audio <- new
  setSrc audio (JSString.pack path)
  waitTillLoads audio
  return audio
