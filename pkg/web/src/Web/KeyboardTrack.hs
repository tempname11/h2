module Web.KeyboardTrack (
  new,
  disable,
  readLatest,
  Ref
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Web
import Heroes.Input                                      (Key (..))
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.JSString                             as JSString
import JavaScript.Object                                 (unsafeGetProp)
import JavaScript.Object                                 (Object)
import System.IO.Unsafe                                  (unsafePerformIO)
import GHCJS.Types                                       (isUndefined)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

newtype Ref = Ref JSVal
instance IsJSVal Ref

foreign import javascript unsafe "h$newKeyboardTrackRef()"
  new :: IO Ref

foreign import javascript unsafe "h$disableKeyboardTrackRef($1)"
  disable :: Ref -> IO ()

foreign import javascript unsafe "h$readKeyboardTrackRef($1)"
  readMap :: Ref -> IO Object

readLatest :: Ref -> IO (Key -> Bool)
readLatest ref = do
  obj <- readMap ref
  let pressed key = not $ isUndefined $ unsafePerformIO $
                      unsafeGetProp (jsStringOf key) obj
  return pressed
  
jsStringOf :: Key -> JSString
jsStringOf = JSString.pack . prefix . go
  where
  prefix = ("keyCode:" <>)
  go Escape = "27"
  go R = "82"
  go Left = "37"
  go Right = "39"
  go N1 = "49"
  go N2 = "50"
  go N3  = "51"
