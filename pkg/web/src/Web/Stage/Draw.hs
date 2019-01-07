{-# OPTIONS_GHC -Wno-orphans #-}
module Web.Stage.Draw () where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes.StaticResources                            (StaticResources(..))
import Heroes.UI                                         (fieldCenter)
import Stage.Draw
import Web
import Web.Drawing                                       (CopySpec(..))
import Web.Platform
import qualified GLES                                      as GL
import qualified Heroes.Cell                               as Cell
import qualified Heroes.Platform                           as Platform
import qualified Web.Drawing.OneColor                      as OneColor
import qualified Web.Drawing.Paletted                      as Paletted
import qualified Web.Drawing.Regular                       as Regular
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified JavaScript.Web.AnimationFrame             as AF
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

instance Draw where
  with (deps@Deps {..}) next = do
    let
      WebRenderer ctx qBuffer = renderer
    Regular.with ctx qBuffer $ \regular ->
      Paletted.with ctx qBuffer $ \paletted ->
      OneColor.with ctx qBuffer $ \oneColor ->
      next $ run regular paletted oneColor ctx deps

--------------------------------------------------------------------------------

{-
  darkHexes :: [Hex],
  extraColor :: FighterId -> Maybe Color,
  lightHexes :: [Hex],
  loaded :: Loaded,
  scene :: Scene
-}

run ::
  With (Handler Regular.Cmd) ->
  With (Handler Paletted.Cmd) ->
  With (Handler OneColor.Cmd) ->
  GL.Ctx ->
  Deps ->
  In ->
  IO ()
run regular paletted oneColor ctx (Deps {..}) (In {..}) = do
  let
    curtain = scene ^. curtain_
    StaticResources {..} = staticResources
    --
    bgCmd = fullCopy background 0
    outline = toRegularCmd cellOutline <$> darkHexes
    shaded = toRegularCmd cellShaded <$> lightHexes
    regularCmds = [bgCmd] <> shaded <> outline
    toRegularCmd sprite hex =
      fullCopy sprite $
        (<§>) (fieldCenter .+^ Cell.fromHex hex)
    --
    palettedCmds = []
  --
  GL.glEnable ctx GL.gl_BLEND
  GL.glBlendFunc ctx GL.gl_SRC_ALPHA GL.gl_ONE_MINUS_SRC_ALPHA
  --
  regular $ \draw ->
    for_ regularCmds draw
  --
  paletted $ \draw ->
    for_ palettedCmds draw
  --
  oneColor $ \draw -> do
    let color = V4 0 0 0 curtain
    draw $ OneColor.Cmd { color, box = Nothing, place = 0 }
  --
  void $ AF.waitForAnimationFrame -- XXX does not belong here?

fullCopy :: Platform.StaticSprite -> Point V2 Float -> Regular.Cmd
fullCopy sprite screenPlace = Regular.Cmd sprite spec
  where
  spec = CopySpec {
    box = sprite ^. dimensions_,
    screenBox = sprite ^. dimensions_,
    place = 0,
    screenPlace
  }
