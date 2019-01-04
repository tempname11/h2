module Web.SystemLibraries (
  with,
  Prov (..),
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes.UI                                         (viewportSize)
import Web
import qualified Web.Drawing.Quad                          as Quad
import qualified Web.GLES                                  as GL
import qualified Web.Stage.Links                           as L
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified JavaScript.Web.Canvas                     as Canvas
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Prov = Prov {
  theContext :: L.TheContext,
  theCanvas  :: L.TheCanvas,
  qBuffer    :: L.QBuffer
}

--------------------------------------------------------------------------------

foreign import javascript unsafe "document.body.appendChild($1)"
  appendCanvasToBody :: Canvas -> IO ()

with :: (Prov -> IO a) -> IO a
with next = do
  theCanvas <- Canvas.create (viewportSize ^. _x) (viewportSize ^. _y)
  appendCanvasToBody theCanvas
  theContext <- GL.getWebGLContext theCanvas
  qBuffer <- Quad.createBuffer theContext
  next $ Prov {..}
