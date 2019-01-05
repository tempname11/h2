module Web.Stage.DrawEverything_ (
  with,
  Deps (..),
  In (..),
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Common.With
import Web
import Web.DrawingAct                                    (DrawingAct(..))
import qualified Web.Drawing.OneColor                      as OneColor
import qualified Web.Drawing.Paletted                      as Paletted
import qualified Web.Drawing.Regular                       as Regular
import qualified Web.GLES                                  as GL
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified JavaScript.Web.AnimationFrame             as AF
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Deps = Deps {
  theContext :: L.TheContext,
  qBuffer :: L.QBuffer
}

data In = In {
  drawingAct :: DrawingAct
}

--------------------------------------------------------------------------------

with :: Deps -> ((In -> IO ()) -> IO ()) -> IO ()
with (Deps {..}) next = do
  let ctx = theContext
  Regular.with ctx qBuffer $ \regular ->
    Paletted.with ctx qBuffer $ \paletted ->
    OneColor.with ctx qBuffer $ \oneColor ->
      next $ run regular paletted oneColor theContext

--------------------------------------------------------------------------------

run ::
  With (Handler Regular.Cmd) ->
  With (Handler Paletted.Cmd) ->
  With (Handler OneColor.Cmd) ->
  GL.Context ->
  In ->
  IO ()
run regular paletted oneColor ctx (In {..}) = do

  -- move this?
  GL.enable ctx GL.gl_BLEND
  GL.blendFunc ctx GL.gl_SRC_ALPHA GL.gl_ONE_MINUS_SRC_ALPHA

  let DrawingAct { curtain, palettedCmds, regularCmds } = drawingAct

  regular $ \draw ->
    for_ regularCmds draw

  paletted $ \draw ->
    for_ palettedCmds draw

  oneColor $ \draw -> do
    let color = V4 0 0 0 curtain
    draw $ OneColor.Cmd { color, box = Nothing, place = 0 }

  void $ AF.waitForAnimationFrame -- XXX does not belong here
