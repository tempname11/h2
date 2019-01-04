module Web (
  Image,
  Canvas,
  JSString,
  JSVal,
  IsJSVal,
  fromJSVal,
  {-
  Callback,
  asyncCallback,
  releaseCallback,
  -}
  simpleXHR,
  loadString,
  inspect,
  module Common.IO,
  module Heroes,
) where

{-
import GHCJS.Foreign.Callback (Callback)
import GHCJS.Foreign.Callback (asyncCallback)
import GHCJS.Foreign.Callback (releaseCallback)
-}
-- use "foreign import javascript interruptible" instead


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Common.IO
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified JavaScript.Web.XMLHttpRequest             as XHR
import qualified Data.JSString                             as JSString
import Data.JSString                                     (JSString)
import GHCJS.Marshal                                     (fromJSVal)
import GHCJS.Types                                       (IsJSVal)
import GHCJS.Types                                       (JSVal)
import JavaScript.Web.Canvas                             (Image, Canvas)
import Unsafe.Coerce                                     (unsafeCoerce)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

foreign import javascript unsafe "console.log($1)"
  consoleLog :: JSVal -> IO ()

simpleXHR :: String -> XHR.Request
simpleXHR uri = XHR.Request {
    XHR.reqMethod = XHR.GET,
    XHR.reqURI = JSString.pack uri,
    XHR.reqLogin = Nothing,
    XHR.reqHeaders = [(JSString.pack "Pragma", JSString.pack "no-cache")],
    XHR.reqWithCredentials = False,
    XHR.reqData = XHR.NoData
  }

loadString :: String -> IO String
loadString uri = do
  let request = simpleXHR uri

  result <- XHR.contents <$> XHR.xhrString request
  case result of
    Nothing  -> raise "result is Nothing"
    Just str -> return str

inspect :: a -> IO ()
inspect = consoleLog . unsafeCoerce

