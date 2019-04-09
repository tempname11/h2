{-# OPTIONS_GHC -Wno-orphans #-}
module Web.WND'Canvas () where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes.WND
import Heroes.UI                                         (viewportSize)
import Web
import Web.Platform ()
import qualified Heroes.Cursor                             as Cursor
import qualified Heroes.FilePath                           as FilePath
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import JavaScript.Web.Canvas                             (Canvas)
import qualified Data.JSString                             as JSString
import qualified JavaScript.Web.AnimationFrame             as AF
import qualified JavaScript.Web.Canvas                     as Canvas
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

instance WND where
  type Window = Canvas
  with next = do
    -- not to be confused with the browser's `window`
    window <- Canvas.create (viewportSize ^. _x) (viewportSize ^. _y)
    appendCanvasToBody window
    --
    let
      waitForVsync = void $ AF.waitForAnimationFrame
      changeCursor (In {..}) =
        let
          (fileName, hotspot) = Cursor.metaFor (Cursor.fromIntent intent)
          filePath = FilePath.cursorPathOf fileName
          x = (§) (hotspot ^. _x)
          y = (§) (hotspot ^. _y)
        in
          setCursorStyle window (JSString.pack filePath) x y
    --
    next $ Prov {..}

foreign import javascript unsafe "document.body.appendChild($1)"
  appendCanvasToBody :: Canvas -> IO ()

foreign import javascript unsafe "$1.style.cursor = 'url(' + $2 + ') ' + $3 + ' ' + $4 + ', auto'"
  setCursorStyle :: Canvas -> JSString -> Int -> Int -> IO ()
