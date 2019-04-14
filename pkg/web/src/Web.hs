module Web (
  Canvas,
  IsJSVal,
  JSString,
  JSVal,
  Platform,
  fromJSVal,
  simpleXHR,
  module Heroes,
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Heroes.Platform                                   (Platform)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified JavaScript.Web.XMLHttpRequest             as XHR
import qualified Data.JSString                             as JSString
import Data.JSString                                     (JSString)
import GHCJS.Marshal                                     (fromJSVal)
import GHCJS.Types                                       (IsJSVal)
import GHCJS.Types                                       (JSVal)
import JavaScript.Web.Canvas                             (Canvas)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

simpleXHR :: String -> XHR.Request
simpleXHR uri = XHR.Request {
    XHR.reqMethod = XHR.GET,
    XHR.reqURI = JSString.pack uri,
    XHR.reqLogin = Nothing,
    XHR.reqHeaders = [(JSString.pack "Pragma", JSString.pack "no-cache")],
    XHR.reqWithCredentials = False,
    XHR.reqData = XHR.NoData
  }
