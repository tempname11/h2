{-# OPTIONS_GHC -Wno-orphans #-}
module Web.WND'Canvas () where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes.WND
import Heroes.UI                                         (viewportSize)
import Web
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import JavaScript.Web.Canvas                             (Canvas)
import qualified JavaScript.Web.Canvas                     as Canvas
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

instance WND where
  type Window = Canvas
  type CursorResources = () -- XXX
  with next = do
    let cursorResources = () -- XXX
    window <- Canvas.create (viewportSize ^. _x) (viewportSize ^. _y)
    appendCanvasToBody window
    next $ (, Prov {..}) $ \_ -> do
      return () -- XXX

foreign import javascript unsafe "document.body.appendChild($1)"
  appendCanvasToBody :: Canvas -> IO ()
