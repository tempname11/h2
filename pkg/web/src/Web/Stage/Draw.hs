{-# OPTIONS_GHC -Wno-orphans #-}
module Web.Stage.Draw () where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Animation.Scene                                   (Actor)
import Animation.Scene                                   (Prop)
import Battle                                            (ObstacleId)
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
import qualified Data.Map.Strict                           as M
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

fromActor ::
  Actor ->
  Paletted.Cmd
fromActor actor = Paletted.Cmd sprite spec
  where
  sprite = actor ^. sprite_
  frame = (sprite ^. meta_ . groups_) & (! g) & (! f) -- XXX partial...
  f = actor ^. frameN_
  g = actor ^. groupN_
  -- @copypaste from Native.Stage.PrepareForDrawing_.toCopy
  facing = actor ^. facing_
  screenPlace = (<§>) ((actor ^. position_) .+^ offset)
  offset = (frame ^. offset_) * sign
  sign = case facing of
    West -> V2 (-1) 1
    East -> 1
  spec = CopySpec {
    box = (<§>) (frame ^. box_),
    place = (<§>) (frame ^. place_),
    screenPlace,
    screenBox = (<§>) ((frame ^. box_) * sign)
  }

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
    curtain = 255 * scene ^. curtain_
    comparingY = (comparing . view) (position_ . _y)
    actors = sortBy comparingY $ M.elems $ scene ^. actors_
    props = M.assocs $ scene ^. props_
    StaticResources {..} = staticResources
    --
    bgCmd = fullCopy background 0
    outline = toRegularCmd cellOutline <$> darkHexes
    shaded = toRegularCmd cellShaded <$> lightHexes
    regularCmds = [bgCmd] <> (fromProp <$> props) <> shaded <> outline
    toRegularCmd sprite hex =
      fullCopy sprite $
        (<§>) (fieldCenter .+^ Cell.fromHex hex)
    --
    fromProp :: (ObstacleId, Prop) -> Regular.Cmd
    fromProp (o, prop) = Regular.Cmd sprite spec
      where
      sprite = obstacles (o ^. otype_)
      sign = case prop ^. facing_ of
        West -> V2 (-1) 1
        East -> 1
      screenPlace = (<§>) (prop ^. position_)
      spec = CopySpec {
        box = sprite ^. dimensions_,
        place = 0,
        screenPlace,
        screenBox = (sprite ^. dimensions_) * sign
      }
    --
    palettedCmds = fromActor <$> actors
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
