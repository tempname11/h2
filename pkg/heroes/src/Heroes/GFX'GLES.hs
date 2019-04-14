{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Heroes.GFX'GLES () where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Animation.Scene                                   (Actor)
import Animation.Scene                                   (Handle(..))
import Animation.Scene                                   (Prop)
import Battle                                            (FighterId)
import Battle                                            (ObstacleId)
import Battle                                            (_otype)
import GLES                                              (GLES)
import Heroes
import Heroes.Drawing                                    (CopySpec(..))
import Heroes.Drawing                                    (StaticSprite(..))
import Heroes.Drawing.Utilities                          (makeTexture)
import Heroes.Drawing.Utilities                          (makePaletteTexture)
import Heroes.Drawing.Quad                               (QBuffer)
import Heroes.GFX
import Heroes.H3.Misc                                    (oImgName)
import Heroes.Platform                                   (Platform)
import Heroes.UI                                         (fieldCenter)
import Heroes.UI                                         (Color)
import Heroes.UI                                         (transparent)
import qualified GLES                                      as GL
import qualified Heroes.Cell                               as Cell
import qualified Heroes.Drawing                            as Drawing
import qualified Heroes.Drawing.OneColor                   as OneColor
import qualified Heroes.Drawing.Paletted                   as Paletted
import qualified Heroes.Drawing.Quad                       as Quad
import qualified Heroes.Drawing.Regular                    as Regular
import qualified Heroes.Image                              as Image
import qualified Heroes.FilePath                           as FilePath
import qualified Heroes.Platform                           as Platform
import qualified Heroes.Memory                             as Memory
import qualified Heroes.GLX                                as GLX
import qualified Heroes.WND                                as WND
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.Map.Strict                           as M
import qualified Data.Vector                               as V
import qualified Data.Vector.Storable                      as SV
import qualified Data.Vector.Generic                       as GV
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Renderer'GLES = Renderer'GLES GL.Ctx QBuffer

instance GFX'Types where
  type StaticSprite = Drawing.StaticSprite
  type ComplexSprite = Drawing.ComplexSprite

instance (GLES, Platform, Memory.Memory, GLX.GLX, WND.WND, GFX'Types) => GFX where
  type Renderer = Renderer'GLES
  with (Deps {..}) next = do
    ctx <- GLX.getGLContext window
    qBuffer <- Quad.createBuffer ctx
    background <- loadStatic ctx FilePath.background
    cellShaded <- loadStatic ctx FilePath.cellShaded
    cellOutline <- loadStatic ctx FilePath.cellOutline
    --
    let
      allObstacles = [minBound .. maxBound]
    --
    obstacleResourceMap <- (M.fromList <$>) $ for allObstacles $ \k -> do
      v <- loadStatic ctx $ FilePath.staticPathOf (oImgName k)
      return (k, v)
    --
    let
      obstacles t = obstacleResourceMap M.! t
      renderer = Renderer'GLES ctx qBuffer
      staticResources = StaticResources {..}
    --
    Regular.with ctx qBuffer $ \regular ->
      Paletted.with ctx qBuffer $ \paletted ->
      OneColor.with ctx qBuffer $ \oneColor -> do
        let draw = run regular paletted oneColor ctx staticResources
        next $ Prov {..}
  --
  loadComplexSprite (Renderer'GLES ctx _) meta path = do
    image <- Platform.loadImage path >>= \case
      Right image -> return image
      Left str -> raise str
    atlasTexture <- makeTexture ctx GL.gl_R8 GL.gl_RED image
    paletteArray <- GLX.generatePaletteArray (meta ^. _palette)
    paletteTexture <- makePaletteTexture ctx paletteArray
    return $ Drawing.ComplexSprite { .. }
  --
  destroyComplexSprite _ = return () -- XXX

loadStatic ::
  (Platform, GL.GLES) =>
  GL.Ctx ->
  String ->
  IO Drawing.StaticSprite
loadStatic ctx path = do
  image <- Platform.loadImage path >>= \case
    Right image -> return image
    Left str -> raise str
  w <- Image.width image
  h <- Image.height image
  texture <- makeTexture ctx GL.gl_RGBA GL.gl_RGBA image
  let dimensions = (<§>) $ V2 w h
  return $ StaticSprite { texture, dimensions }

fromActor ::
  (FighterId -> Maybe Color) ->
  (Handle, Actor) ->
  Paletted.Cmd
fromActor extraColor (h, actor) = Paletted.Cmd sprite spec outlineColor
  where
  outlineColor =
    case h of
      Handle'Fighter fyr -> maybe transparent id (extraColor fyr)
      _ -> transparent
    
  sprite = actor ^. _sprite . _some
  frame = (sprite ^. _meta . _groups) & (! g) & (! f) -- XXX partial...
  f = actor ^. _frameN
  g = actor ^. _groupN
  -- @copypaste from Native.Stage.Prepare.toCopy
  facing = actor ^. _facing
  screenPlace = (<§>) ((actor ^. _position) .+^ offset .-^ (V2 0 (actor ^. _height)))
  offset = (frame ^. _offset) * sign
  sign = case facing of
    West -> V2 (-1) 1
    East -> 1
  spec = CopySpec {
    box = (<§>) (frame ^. _box),
    place = (<§>) (frame ^. _place),
    screenPlace,
    screenBox = (<§>) ((frame ^. _box) * sign)
  }

run ::
  (GL.GLES) =>
  With (Handler Regular.Cmd) ->
  With (Handler Paletted.Cmd) ->
  With (Handler OneColor.Cmd) ->
  GL.Ctx ->
  StaticResources ->
  In ->
  IO ()
run regular paletted oneColor ctx staticResources (In {..}) = do
  let
    comparingY = comparing (view $ _2 . _position . _y)
    actors = sortBy comparingY $ M.assocs $ scene ^. _actors
    props = M.assocs $ scene ^. _props
    StaticResources {..} = staticResources
    bgCmd = background `fullCopyAt` (SV.singleton 0)
    --
    regularCmds =
      [bgCmd] <>
      (fromProp <$> props) <>
      [
        hexCmd cellShaded darkHexes,
        hexCmd cellOutline lightHexes
      ]
    --
    hexCmd :: Drawing.StaticSprite -> V.Vector Hex -> Regular.Cmd
    hexCmd sprite hexes =
      sprite `fullCopyAt`
        GV.convert ((\hex -> (<§>) (fieldCenter .+^ Cell.fromHex hex)) `GV.map` hexes)
    --
    fromProp :: (ObstacleId, Prop) -> Regular.Cmd
    fromProp (o, prop) = cmd
      where
      sprite = obstacles (o ^. _otype)
      sign = case prop ^. _facing of
        West -> V2 (-1) 1
        East -> 1
      screenPlace = (<§>) (prop ^. _position)
      cmd = Regular.Cmd {
        sprite,
        box = sprite ^. _dimensions,
        place = 0,
        screenPlaces = SV.singleton screenPlace,
        screenBox = (sprite ^. _dimensions) * sign
      }
    --
    palettedCmds = fromActor extraColor <$> actors
  --
  Drawing.clear ctx
  GL.glEnable ctx GL.gl_BLEND
  GL.glBlendFuncSeparate ctx
    GL.gl_SRC_ALPHA GL.gl_ONE_MINUS_SRC_ALPHA
    GL.gl_ONE GL.gl_ONE_MINUS_SRC_ALPHA
  --
  regular $ \draw ->
    for_ regularCmds draw
  --
  paletted $ \draw ->
    for_ palettedCmds draw
  --
  oneColor $ \draw -> do
    let color = V4 0 0 0 (floor . (255 *) $ (scene ^. _curtain))
    draw $ OneColor.Cmd { color, box = Nothing, place = 0 }

fullCopyAt :: Drawing.StaticSprite -> SV.Vector (Point V2 Float) -> Regular.Cmd
fullCopyAt sprite screenPlaces =
  Regular.Cmd {
    sprite,
    box = sprite ^. _dimensions,
    screenBox = sprite ^. _dimensions,
    place = 0,
    screenPlaces
  }
